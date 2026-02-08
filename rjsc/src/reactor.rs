use std::future::Future;
use std::pin::Pin;
use std::ptr;
use std::sync::Arc;
use std::task::{
    Context as TaskContext, Poll, RawWaker, RawWakerVTable, Wake, Waker,
};

use crate::{Context, Exception};

/// Status returned by the reactor after polling.
pub enum PollStatus {
    /// More work remains, reactor should be polled again later.
    Pending,
    /// All work is complete.
    Ready,
}

/// A pluggable event loop reactor that drives JavaScript execution.
///
/// The reactor is responsible for:
/// - Draining JavaScript microtasks
/// - Integrating with external async runtimes (tokio, async-std, smol, etc.)
/// - Notifying when async work completes
///
/// This trait is object-safe (dyn-compatible) to allow runtime polymorphism.
pub trait Reactor: std::any::Any {
    /// Poll the reactor, driving both JS microtasks and external I/O forward.
    ///
    /// This method should process any pending work and return whether more
    /// work remains to be done.
    fn poll(&self, ctx: &Context) -> Result<PollStatus, Exception>;

    /// Notify the reactor that a Promise has been resolved/rejected.
    ///
    /// Called by PromiseFuture when the JS Promise settles, allowing the
    /// reactor to wake any waiting tasks.
    fn notify(&self);

    /// Return self as Any for downcasting.
    fn as_any(&self) -> &dyn std::any::Any;
}

/// A noop reactor that only drains JSC microtasks.
///
/// This is the simplest possible reactor and is used as the default
/// when no external event loop integration is needed.
pub struct MicrotaskReactor {
    noop: rjsc_sys::JSStringRef,
}

impl MicrotaskReactor {
    /// Create a new microtask-only reactor.
    pub fn new() -> Self {
        let noop = unsafe { crate::js_string_from_rust("0") };
        MicrotaskReactor { noop }
    }

    fn drain_microtasks(&self, ctx: &Context) {
        unsafe {
            rjsc_sys::JSEvaluateScript(
                ctx.as_ctx(),
                self.noop,
                ptr::null_mut(),
                ptr::null_mut(),
                0,
                ptr::null_mut(),
            );
        }
    }

    /// Block on a future until it completes using this reactor.
    ///
    /// This is a convenience method for simple use cases. More complex
    /// reactors should implement their own blocking strategy.
    pub fn block_on<F: Future + Unpin>(
        &self,
        ctx: &Context,
        mut fut: F,
    ) -> F::Output {
        let waker = noop_waker();
        let mut cx = TaskContext::from_waker(&waker);

        loop {
            match Pin::new(&mut fut).poll(&mut cx) {
                Poll::Ready(val) => return val,
                Poll::Pending => {
                    self.drain_microtasks(ctx);
                }
            }
        }
    }
}

impl Default for MicrotaskReactor {
    fn default() -> Self {
        Self::new()
    }
}

impl Reactor for MicrotaskReactor {
    fn poll(&self, ctx: &Context) -> Result<PollStatus, Exception> {
        self.drain_microtasks(ctx);
        Ok(PollStatus::Ready)
    }

    fn notify(&self) {
        // No-op for the microtask reactor
        // The block_on loop handles polling
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Drop for MicrotaskReactor {
    fn drop(&mut self) {
        unsafe { rjsc_sys::JSStringRelease(self.noop) };
    }
}

/// A waker that does nothing when woken.
struct NoopWaker;

impl Wake for NoopWaker {
    fn wake(self: Arc<Self>) {}
    fn wake_by_ref(self: &Arc<Self>) {}
}

/// Create a noop waker for simple polling.
fn noop_waker() -> Waker {
    let waker = Arc::new(NoopWaker);
    let raw_waker =
        RawWaker::new(Arc::into_raw(waker) as *const (), &NOOP_VTABLE);
    unsafe { Waker::from_raw(raw_waker) }
}

static NOOP_VTABLE: RawWakerVTable = RawWakerVTable::new(
    |ptr| unsafe {
        let waker: Arc<NoopWaker> = Arc::from_raw(ptr as *const NoopWaker);
        std::mem::forget(Arc::clone(&waker));
        RawWaker::new(ptr, &NOOP_VTABLE)
    },
    |ptr| unsafe {
        let _: Arc<NoopWaker> = Arc::from_raw(ptr as *const NoopWaker);
    },
    |ptr| unsafe {
        let _: Arc<NoopWaker> = Arc::from_raw(ptr as *const NoopWaker);
    },
    |ptr| unsafe {
        let _: Arc<NoopWaker> = Arc::from_raw(ptr as *const NoopWaker);
    },
);

use std::cell::RefCell;
use std::collections::VecDeque;
use std::future::Future;
use std::rc::Rc;
use std::sync::Arc;
use std::task::{
    Context as TaskContext, Poll, RawWaker, RawWakerVTable, Wake, Waker,
};

use rjsc_sys::*;

use crate::reactor::{MicrotaskReactor, PollStatus, Reactor};

/// A JavaScript runtime (context group).
///
/// Contexts created from the same runtime share a VM and
/// must remain on the same thread.
pub struct Runtime {
    inner: Rc<RuntimeInner>,
}

impl Runtime {
    /// Creates a new JavaScript runtime with the given reactor.
    ///
    /// The reactor is responsible for driving the event loop,
    /// draining microtasks, and integrating with external async
    /// runtimes.
    pub fn new(reactor: Box<dyn Reactor>) -> Self {
        let raw = unsafe { JSContextGroupCreate() };
        Runtime {
            inner: Rc::new(RuntimeInner {
                raw,
                tasks: RefCell::new(VecDeque::new()),
                reactor,
            }),
        }
    }

    /// Creates a new JavaScript runtime with a default microtask-only reactor.
    ///
    /// This is useful for simple use cases that don't need external
    /// event loop integration.
    pub fn default() -> Self {
        Self::new(Box::new(MicrotaskReactor::new()))
    }

    pub(crate) fn raw(&self) -> JSContextGroupRef {
        self.inner.raw
    }

    /// Creates a new context inside this runtime.
    pub fn new_context(&self) -> crate::Context {
        crate::Context::new_in(self)
    }

    pub(crate) fn inner_clone(&self) -> Rc<RuntimeInner> {
        Rc::clone(&self.inner)
    }

    /// Access the reactor for this runtime.
    pub fn with_reactor<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&dyn Reactor) -> R,
    {
        f(&*self.inner.reactor)
    }

    /// Poll the runtime, driving both JS microtasks and async tasks forward.
    ///
    /// Returns whether more work remains to be done.
    pub fn poll(
        &self,
        ctx: &crate::Context,
    ) -> Result<PollStatus, crate::Exception> {
        self.with_reactor(|r| r.poll(ctx))
    }

    /// Block on a future until it completes.
    ///
    /// This integrates with the reactor's event loop to drive
    /// the future to completion.
    pub fn block_on<F: Future + Unpin>(
        &self,
        ctx: &crate::Context,
        fut: F,
    ) -> F::Output {
        // Try to downcast to MicrotaskReactor for its efficient block_on
        // Otherwise fall back to generic polling
        self.with_reactor(|r| {
            // Safe because we're just checking the type
            if let Some(mt) = r.as_any().downcast_ref::<MicrotaskReactor>() {
                mt.block_on(ctx, fut)
            } else {
                // Generic implementation that polls the reactor
                let waker = noop_waker();
                let mut cx = TaskContext::from_waker(&waker);
                let mut fut = std::pin::pin!(fut);

                loop {
                    match fut.as_mut().poll(&mut cx) {
                        Poll::Ready(val) => return val,
                        Poll::Pending => {
                            if let Err(e) = r.poll(ctx) {
                                panic!("Reactor poll failed: {:?}", e);
                            }
                        }
                    }
                }
            }
        })
    }

    /// Returns true if the runtime has any pending tasks.
    pub fn has_pending_tasks(&self) -> bool {
        !self.inner.tasks.borrow().is_empty()
    }
}

pub(crate) struct RuntimeInner {
    pub(crate) raw: JSContextGroupRef,
    pub(crate) tasks: RefCell<VecDeque<crate::task::Task>>,
    pub(crate) reactor: Box<dyn Reactor>,
}

impl RuntimeInner {
    pub(crate) fn push_task(&self, task: crate::task::Task) {
        self.tasks.borrow_mut().push_back(task);
    }

    pub(crate) fn poll_tasks(&self, ctx: &crate::Context) -> usize {
        let mut pending = self.tasks.borrow_mut();
        let mut completed = 0usize;
        let mut remaining = VecDeque::new();

        while let Some(mut task) = pending.pop_front() {
            match task.poll(ctx) {
                std::task::Poll::Ready(()) => completed += 1,
                std::task::Poll::Pending => remaining.push_back(task),
            }
        }

        *pending = remaining;
        completed
    }

    pub(crate) fn with_reactor<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&dyn Reactor) -> R,
    {
        f(&*self.reactor)
    }
}

impl Drop for RuntimeInner {
    fn drop(&mut self) {
        unsafe { JSContextGroupRelease(self.raw) };
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

use std::future::Future;
use std::pin::Pin;
use std::task::{
    Context as TaskContext, Poll, RawWaker, RawWakerVTable, Waker,
};

use crate::owned::ValueOwned;
use crate::promise::PromiseResolverOwned;
use crate::{Context, Value};

#[doc(hidden)]
pub struct Task {
    future: Pin<Box<dyn Future<Output = TaskResult> + 'static>>,
    resolver: PromiseResolverOwned,
}

#[doc(hidden)]
pub enum TaskResult {
    Ok(TaskValue),
    Value(ValueOwned),
    Err(String),
}

#[doc(hidden)]
pub enum TaskValue {
    Undefined,
    Bool(bool),
    F64(f64),
    String(String),
}

impl Task {
    pub fn new(
        future: Pin<Box<dyn Future<Output = TaskResult> + 'static>>,
        resolver: PromiseResolverOwned,
    ) -> Self {
        Task { future, resolver }
    }

    pub(crate) fn poll(&mut self, ctx: &Context) -> Poll<()> {
        let waker = noop_waker();
        let mut cx = TaskContext::from_waker(&waker);

        match self.future.as_mut().poll(&mut cx) {
            Poll::Ready(TaskResult::Ok(val)) => {
                let value = match val {
                    TaskValue::Undefined => Value::undefined(ctx),
                    TaskValue::Bool(v) => Value::from_bool(ctx, v),
                    TaskValue::F64(v) => Value::from_f64(ctx, v),
                    TaskValue::String(v) => Value::from_str(ctx, &v),
                };
                if let Err(err) = self.resolver.resolve(ctx, &value) {
                    self.resolver
                        .reject_str(ctx, &err.message().to_string())
                        .ok();
                }
                Poll::Ready(())
            }
            Poll::Ready(TaskResult::Value(val)) => {
                let value = val.into_value(ctx);
                if let Err(err) = self.resolver.resolve(ctx, &value) {
                    self.resolver
                        .reject_str(ctx, &err.message().to_string())
                        .ok();
                }
                Poll::Ready(())
            }
            Poll::Ready(TaskResult::Err(err)) => {
                self.resolver.reject_str(ctx, &err).ok();
                Poll::Ready(())
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

fn noop_waker() -> Waker {
    unsafe fn clone(_: *const ()) -> RawWaker {
        RawWaker::new(std::ptr::null(), &VTABLE)
    }
    unsafe fn wake(_: *const ()) {}
    unsafe fn wake_by_ref(_: *const ()) {}
    unsafe fn drop(_: *const ()) {}

    static VTABLE: RawWakerVTable =
        RawWakerVTable::new(clone, wake, wake_by_ref, drop);

    unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &VTABLE)) }
}

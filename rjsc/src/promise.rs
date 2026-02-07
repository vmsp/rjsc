use std::cell::{Cell, RefCell};
use std::future::Future;
use std::pin::Pin;
use std::ptr;
use std::rc::Rc;
use std::task::{Context, Poll, Waker};

use rjsc_sys::*;

use crate::{JsContext, JsException, JsObject, JsValue};

/// A JavaScript Promise, tied to the lifetime of its [`JsContext`].
pub struct JsPromise<'ctx> {
    obj: JsObject<'ctx>,
}

impl<'ctx> JsPromise<'ctx> {
    pub(crate) fn from_object(
        ctx: &'ctx JsContext,
        obj: JsObject<'ctx>,
    ) -> Result<Self, JsException> {
        ensure_thenable(ctx, &obj)?;
        Ok(Self { obj })
    }

    pub fn from_value(
        ctx: &'ctx JsContext,
        value: JsValue<'ctx>,
    ) -> Result<Self, JsException> {
        if !value.is_object() {
            return Err(JsException::new(
                "Value is not an object; cannot be a promise.",
            ));
        }
        let obj = value.to_object(ctx)?;
        Self::from_object(ctx, obj)
    }

    pub fn deferred(
        ctx: &'ctx JsContext,
    ) -> Result<(Self, JsPromiseResolver<'ctx>), JsException> {
        let init = "(function() {\
            let resolve; \
            let reject; \
            let promise = new Promise((res, rej) => {\
                resolve = res; \
                reject = rej; \
            }); \
            return { promise, resolve, reject };\
        })()";

        let obj_val = ctx.eval(init)?;
        let obj = obj_val.to_object(ctx)?;

        let promise_val = obj.get("promise", ctx)?;
        let resolve_val = obj.get("resolve", ctx)?;
        let reject_val = obj.get("reject", ctx)?;

        let promise = JsPromise::from_value(ctx, promise_val)?;
        let resolve_obj = resolve_val.to_object(ctx)?;
        let reject_obj = reject_val.to_object(ctx)?;

        let resolver =
            JsPromiseResolver { resolve: resolve_obj, reject: reject_obj };

        Ok((promise, resolver))
    }

    pub fn to_object(&self) -> &JsObject<'ctx> {
        &self.obj
    }

    pub fn to_value(&self, ctx: &'ctx JsContext) -> JsValue<'ctx> {
        self.obj.to_value(ctx)
    }

    pub fn into_future(self, ctx: &'ctx JsContext) -> JsPromiseFuture<'ctx> {
        JsPromiseFuture::new(ctx, self)
    }

    pub fn await_blocking(
        self,
        ctx: &'ctx JsContext,
    ) -> Result<JsValue<'ctx>, JsException> {
        self.await_blocking_with(ctx, &JsMicrotaskDrain::default())
    }

    pub fn await_blocking_with(
        self,
        ctx: &'ctx JsContext,
        driver: &dyn JsJobDriver,
    ) -> Result<JsValue<'ctx>, JsException> {
        let state = Rc::new(PromiseState::new());

        let (on_resolve, on_reject) = build_handlers(ctx, Rc::clone(&state))?;

        self.attach_handlers(ctx, on_resolve.raw, on_reject.raw)?;

        for _ in 0..driver.max_rounds() {
            if state.settled.get() {
                break;
            }
            driver.drain_jobs(ctx)?;
        }

        state.take_result(ctx)
    }

    fn attach_handlers(
        &self,
        ctx: &'ctx JsContext,
        on_resolve: JSObjectRef,
        on_reject: JSObjectRef,
    ) -> Result<(), JsException> {
        let promise_obj = unsafe {
            JSValueToObject(ctx.as_ctx(), self.obj.raw, ptr::null_mut())
        };
        let then_key = unsafe { crate::js_string_from_rust("then") };
        let then_val = unsafe {
            JSObjectGetProperty(
                ctx.as_ctx(),
                promise_obj,
                then_key,
                ptr::null_mut(),
            )
        };
        unsafe { JSStringRelease(then_key) };
        let then_fn =
            unsafe { JSValueToObject(ctx.as_ctx(), then_val, ptr::null_mut()) };

        let args = [on_resolve as JSValueRef, on_reject as JSValueRef];
        unsafe {
            JSObjectCallAsFunction(
                ctx.as_ctx(),
                then_fn,
                promise_obj,
                2,
                args.as_ptr(),
                ptr::null_mut(),
            );
        }
        Ok(())
    }
}

pub struct JsPromiseResolver<'ctx> {
    resolve: JsObject<'ctx>,
    reject: JsObject<'ctx>,
}

impl<'ctx> JsPromiseResolver<'ctx> {
    pub fn resolve(
        &self,
        ctx: &'ctx JsContext,
        value: &JsValue<'ctx>,
    ) -> Result<(), JsException> {
        self.resolve.call(None, &[value], ctx)?;
        Ok(())
    }

    pub fn resolve_undefined(
        &self,
        ctx: &'ctx JsContext,
    ) -> Result<(), JsException> {
        let val = JsValue::undefined(ctx);
        self.resolve(ctx, &val)
    }

    pub fn reject(
        &self,
        ctx: &'ctx JsContext,
        value: &JsValue<'ctx>,
    ) -> Result<(), JsException> {
        self.reject.call(None, &[value], ctx)?;
        Ok(())
    }

    pub fn reject_str(
        &self,
        ctx: &'ctx JsContext,
        message: &str,
    ) -> Result<(), JsException> {
        let val = JsValue::from_str(ctx, message);
        self.reject(ctx, &val)
    }
}

pub trait JsJobDriver {
    fn drain_jobs(&self, ctx: &JsContext) -> Result<(), JsException>;
    fn max_rounds(&self) -> usize;
}

pub struct JsMicrotaskDrain {
    max_rounds: usize,
    noop: JSStringRef,
}

impl JsMicrotaskDrain {
    pub fn new(max_rounds: usize) -> Self {
        let noop = unsafe { crate::js_string_from_rust("0") };
        JsMicrotaskDrain { max_rounds, noop }
    }
}

impl Default for JsMicrotaskDrain {
    fn default() -> Self {
        JsMicrotaskDrain::new(64)
    }
}

impl JsJobDriver for JsMicrotaskDrain {
    fn drain_jobs(&self, ctx: &JsContext) -> Result<(), JsException> {
        unsafe {
            JSEvaluateScript(
                ctx.as_ctx(),
                self.noop,
                ptr::null_mut(),
                ptr::null_mut(),
                0,
                ptr::null_mut(),
            );
        }
        Ok(())
    }

    fn max_rounds(&self) -> usize {
        self.max_rounds
    }
}

impl Drop for JsMicrotaskDrain {
    fn drop(&mut self) {
        unsafe { JSStringRelease(self.noop) };
    }
}

pub struct JsPromiseFuture<'ctx> {
    ctx: &'ctx JsContext,
    promise: JsPromise<'ctx>,
    state: Rc<PromiseState>,
    registered: bool,
    on_resolve: Option<JsObject<'ctx>>,
    on_reject: Option<JsObject<'ctx>>,
}

impl<'ctx> JsPromiseFuture<'ctx> {
    fn new(ctx: &'ctx JsContext, promise: JsPromise<'ctx>) -> Self {
        JsPromiseFuture {
            ctx,
            promise,
            state: Rc::new(PromiseState::new()),
            registered: false,
            on_resolve: None,
            on_reject: None,
        }
    }
}

impl<'ctx> Future for JsPromiseFuture<'ctx> {
    type Output = Result<JsValue<'ctx>, JsException>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        this.state.waker.replace(Some(cx.waker().clone()));

        if !this.registered {
            let handlers = build_handlers(this.ctx, Rc::clone(&this.state));
            let (on_resolve, on_reject) = match handlers {
                Ok(handlers) => handlers,
                Err(err) => return Poll::Ready(Err(err)),
            };

            if let Err(err) = this.promise.attach_handlers(
                this.ctx,
                on_resolve.raw,
                on_reject.raw,
            ) {
                return Poll::Ready(Err(err));
            }

            this.on_resolve = Some(on_resolve);
            this.on_reject = Some(on_reject);
            this.registered = true;
        }

        match this.state.poll_result(this.ctx) {
            Some(result) => Poll::Ready(result),
            None => Poll::Pending,
        }
    }
}

struct PromiseState {
    settled: Cell<bool>,
    resolved: Cell<JSValueRef>,
    rejected: Cell<JSValueRef>,
    waker: RefCell<Option<Waker>>,
}

impl PromiseState {
    fn new() -> Self {
        PromiseState {
            settled: Cell::new(false),
            resolved: Cell::new(ptr::null()),
            rejected: Cell::new(ptr::null()),
            waker: RefCell::new(None),
        }
    }

    fn wake(&self) {
        if let Some(waker) = self.waker.borrow().as_ref() {
            waker.wake_by_ref();
        }
    }

    fn take_result<'ctx>(
        &self,
        ctx: &'ctx JsContext,
    ) -> Result<JsValue<'ctx>, JsException> {
        if !self.settled.get() {
            return Err(JsException::new(
                "Promise did not settle synchronously. \
                    eval_async only supports promises that resolve \
                    via microtasks (no pending I/O or timers).",
            ));
        }

        let rejected_raw = self.rejected.get();
        if !rejected_raw.is_null() {
            let err = JsException::from_jsvalue(ctx.as_ctx(), rejected_raw);
            unsafe { JSValueUnprotect(ctx.as_ctx(), rejected_raw) };
            self.rejected.set(ptr::null());
            return Err(err);
        }

        let resolved_raw = self.resolved.get();
        let result = unsafe { JsValue::from_raw(ctx, resolved_raw) };
        unsafe { JSValueUnprotect(ctx.as_ctx(), resolved_raw) };
        self.resolved.set(ptr::null());
        Ok(result)
    }

    fn poll_result<'ctx>(
        &self,
        ctx: &'ctx JsContext,
    ) -> Option<Result<JsValue<'ctx>, JsException>> {
        if !self.settled.get() {
            return None;
        }
        Some(self.take_result(ctx))
    }
}

fn build_handlers<'ctx>(
    ctx: &'ctx JsContext,
    state: Rc<PromiseState>,
) -> Result<(JsObject<'ctx>, JsObject<'ctx>), JsException> {
    let s = Rc::clone(&state);
    let on_resolve = ctx.make_callback(move |ctx_raw, args| {
        let val = if args.is_null() {
            unsafe { JSValueMakeUndefined(ctx_raw) }
        } else {
            unsafe { *args }
        };
        unsafe { JSValueProtect(ctx_raw, val) };
        s.resolved.set(val);
        s.settled.set(true);
        s.wake();
        unsafe { JSValueMakeUndefined(ctx_raw) }
    });

    let s = Rc::clone(&state);
    let on_reject = ctx.make_callback(move |ctx_raw, args| {
        let val = if args.is_null() {
            unsafe { JSValueMakeUndefined(ctx_raw) }
        } else {
            unsafe { *args }
        };
        unsafe { JSValueProtect(ctx_raw, val) };
        s.rejected.set(val);
        s.settled.set(true);
        s.wake();
        unsafe { JSValueMakeUndefined(ctx_raw) }
    });

    let on_resolve_obj = unsafe { JsObject::from_raw(ctx, on_resolve) };
    let on_reject_obj = unsafe { JsObject::from_raw(ctx, on_reject) };
    Ok((on_resolve_obj, on_reject_obj))
}

fn ensure_thenable(
    ctx: &JsContext,
    obj: &JsObject<'_>,
) -> Result<(), JsException> {
    let then_val = obj.get("then", ctx)?;
    if !then_val.is_object() {
        return Err(JsException::new(
            "Value is not thenable (missing Promise.then).",
        ));
    }
    let then_obj = then_val.to_object(ctx)?;
    if !then_obj.is_function() {
        return Err(JsException::new(
            "Value is not thenable (Promise.then not callable).",
        ));
    }
    Ok(())
}

use std::cell::{Cell, RefCell};
use std::future::Future;
use std::pin::Pin;
use std::ptr;
use std::rc::Rc;
use std::task::{Context as TaskContext, Poll, Waker};

use rjsc_sys::*;

use crate::{Context, Exception, Object, Value};

/// A JavaScript Promise, tied to the lifetime of its
/// [`Context`].
pub struct Promise<'ctx> {
    obj: Object<'ctx>,
}

impl<'ctx> Promise<'ctx> {
    pub(crate) fn from_object(obj: Object<'ctx>) -> Result<Self, Exception> {
        ensure_thenable(&obj)?;
        Ok(Self { obj })
    }

    pub fn from_value(value: Value<'ctx>) -> Result<Self, Exception> {
        if !value.is_object() {
            return Err(Exception::new(
                "Value is not an object; cannot be a promise.",
            ));
        }
        let obj = value.to_object()?;
        Self::from_object(obj)
    }

    pub fn deferred(
        ctx: &'ctx Context,
    ) -> Result<(Self, PromiseResolver<'ctx>), Exception> {
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
        let obj = obj_val.to_object()?;

        let promise_val = obj.get("promise")?;
        let resolve_val = obj.get("resolve")?;
        let reject_val = obj.get("reject")?;

        let promise = Promise::from_value(promise_val)?;
        let resolve_obj = resolve_val.to_object()?;
        let reject_obj = reject_val.to_object()?;

        let resolver =
            PromiseResolver { resolve: resolve_obj, reject: reject_obj };

        Ok((promise, resolver))
    }

    pub fn to_object(&self) -> &Object<'ctx> {
        &self.obj
    }

    pub fn to_value(&self) -> Value<'ctx> {
        self.obj.to_value()
    }

    /// Convert this Promise into a Rust Future.
    ///
    /// The returned future will resolve when the JavaScript Promise
    /// settles. Use this with a reactor's `block_on` method or
    /// integrate with your async runtime.
    pub fn into_future(self) -> PromiseFuture<'ctx> {
        PromiseFuture::new(self)
    }

    fn attach_handlers(
        &self,
        on_resolve: JSObjectRef,
        on_reject: JSObjectRef,
    ) -> Result<(), Exception> {
        let ctx = self.obj.ctx;
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

pub struct PromiseResolver<'ctx> {
    resolve: Object<'ctx>,
    reject: Object<'ctx>,
}

impl<'ctx> PromiseResolver<'ctx> {
    pub fn resolve(&self, value: &Value<'ctx>) -> Result<(), Exception> {
        self.resolve.call(None, &[value])?;
        Ok(())
    }

    pub fn resolve_undefined(&self) -> Result<(), Exception> {
        let val = Value::undefined(self.resolve.ctx);
        self.resolve(&val)
    }

    pub fn reject(&self, value: &Value<'ctx>) -> Result<(), Exception> {
        self.reject.call(None, &[value])?;
        Ok(())
    }

    pub fn reject_str(&self, message: &str) -> Result<(), Exception> {
        let val = Value::from_str(self.resolve.ctx, message);
        self.reject(&val)
    }

    #[doc(hidden)]
    pub fn to_owned(&self) -> PromiseResolverOwned {
        let ctx = self.resolve.ctx;
        let resolve = self.resolve.raw;
        let reject = self.reject.raw;
        unsafe { JSValueProtect(ctx.as_ctx(), resolve) };
        unsafe { JSValueProtect(ctx.as_ctx(), reject) };
        PromiseResolverOwned { ctx: ctx.as_ctx(), resolve, reject }
    }
}

#[doc(hidden)]
pub struct PromiseResolverOwned {
    ctx: JSContextRef,
    resolve: JSObjectRef,
    reject: JSObjectRef,
}

impl PromiseResolverOwned {
    pub(crate) fn resolve(
        &self,
        ctx: &Context,
        value: &Value<'_>,
    ) -> Result<(), Exception> {
        self.resolve_raw(ctx, value.raw)
    }

    pub(crate) fn resolve_raw(
        &self,
        ctx: &Context,
        value: JSValueRef,
    ) -> Result<(), Exception> {
        let mut exception: JSValueRef = ptr::null();
        let args = [value];
        unsafe {
            JSObjectCallAsFunction(
                ctx.as_ctx(),
                self.resolve,
                ptr::null_mut(),
                1,
                args.as_ptr(),
                &mut exception,
            );
        }
        if !exception.is_null() {
            return Err(Exception::from_jsvalue(ctx.as_ctx(), exception));
        }
        Ok(())
    }

    pub(crate) fn reject_str(
        &self,
        ctx: &Context,
        message: &str,
    ) -> Result<(), Exception> {
        let js_msg = unsafe {
            let s = crate::js_string_from_rust(message);
            let v = JSValueMakeString(ctx.as_ctx(), s);
            JSStringRelease(s);
            v
        };
        let mut exception: JSValueRef = ptr::null();
        let args = [js_msg];
        unsafe {
            JSObjectCallAsFunction(
                ctx.as_ctx(),
                self.reject,
                ptr::null_mut(),
                1,
                args.as_ptr(),
                &mut exception,
            );
        }
        if !exception.is_null() {
            return Err(Exception::from_jsvalue(ctx.as_ctx(), exception));
        }
        Ok(())
    }
}

impl Drop for PromiseResolverOwned {
    fn drop(&mut self) {
        unsafe { JSValueUnprotect(self.ctx, self.resolve) };
        unsafe { JSValueUnprotect(self.ctx, self.reject) };
    }
}

/// A Future that resolves when a JavaScript Promise settles.
pub struct PromiseFuture<'ctx> {
    ctx: &'ctx Context,
    promise: Promise<'ctx>,
    state: Rc<PromiseState>,
    registered: bool,
    on_resolve: Option<Object<'ctx>>,
    on_reject: Option<Object<'ctx>>,
}

impl<'ctx> PromiseFuture<'ctx> {
    fn new(promise: Promise<'ctx>) -> Self {
        let ctx = promise.obj.ctx;
        PromiseFuture {
            ctx,
            promise,
            state: Rc::new(PromiseState::new()),
            registered: false,
            on_resolve: None,
            on_reject: None,
        }
    }
}

impl<'ctx> Future for PromiseFuture<'ctx> {
    type Output = Result<Value<'ctx>, Exception>;

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut TaskContext<'_>,
    ) -> Poll<Self::Output> {
        let this = self.get_mut();
        this.state.waker.replace(Some(cx.waker().clone()));

        if !this.registered {
            let handlers = build_handlers(this.ctx, Rc::clone(&this.state));
            let (on_resolve, on_reject) = match handlers {
                Ok(handlers) => handlers,
                Err(err) => return Poll::Ready(Err(err)),
            };

            if let Err(err) =
                this.promise.attach_handlers(on_resolve.raw, on_reject.raw)
            {
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

    fn wake(&self, ctx: &Context) {
        if let Some(waker) = self.waker.borrow().as_ref() {
            waker.wake_by_ref();
        }
        // Notify the reactor that a promise has settled
        ctx.notify_reactor();
    }

    fn take_result<'ctx>(
        &self,
        ctx: &'ctx Context,
    ) -> Result<Value<'ctx>, Exception> {
        let rejected_raw = self.rejected.get();
        if !rejected_raw.is_null() {
            let err = Exception::from_jsvalue(ctx.as_ctx(), rejected_raw);
            unsafe { JSValueUnprotect(ctx.as_ctx(), rejected_raw) };
            self.rejected.set(ptr::null());
            return Err(err);
        }

        let resolved_raw = self.resolved.get();
        let result = unsafe { Value::from_raw(ctx, resolved_raw) };
        unsafe { JSValueUnprotect(ctx.as_ctx(), resolved_raw) };
        self.resolved.set(ptr::null());
        Ok(result)
    }

    fn poll_result<'ctx>(
        &self,
        ctx: &'ctx Context,
    ) -> Option<Result<Value<'ctx>, Exception>> {
        if !self.settled.get() {
            return None;
        }
        Some(self.take_result(ctx))
    }
}

fn build_handlers<'ctx>(
    ctx: &'ctx Context,
    state: Rc<PromiseState>,
) -> Result<(Object<'ctx>, Object<'ctx>), Exception> {
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
        // We need to wake with context, but we only have ctx_raw here
        // The wake will be called on next poll via waker
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
        unsafe { JSValueMakeUndefined(ctx_raw) }
    });

    let on_resolve_obj = unsafe { Object::from_raw(ctx, on_resolve) };
    let on_reject_obj = unsafe { Object::from_raw(ctx, on_reject) };
    Ok((on_resolve_obj, on_reject_obj))
}

fn ensure_thenable(obj: &Object<'_>) -> Result<(), Exception> {
    let then_val = obj.get("then")?;
    if !then_val.is_object() {
        return Err(Exception::new(
            "Value is not thenable (missing Promise.then).",
        ));
    }
    let then_obj = then_val.to_object()?;
    if !then_obj.is_function() {
        return Err(Exception::new(
            "Value is not thenable \
             (Promise.then not callable).",
        ));
    }
    Ok(())
}

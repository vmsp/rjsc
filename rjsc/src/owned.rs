use std::mem;
use std::ops::Deref;
use std::rc::Rc;

use rjsc_sys::*;

use crate::{Context, Object, Value};

pub struct ContextOwned {
    ctx: Context,
}

impl ContextOwned {
    pub fn from_ctx(ctx: &Context) -> Self {
        let raw = unsafe { JSGlobalContextRetain(ctx.raw()) };
        let runtime = ctx._runtime.clone();
        let ctx = Context {
            raw,
            _not_send_sync: std::marker::PhantomData,
            _runtime: runtime,
        };
        ContextOwned { ctx }
    }
}

impl Deref for ContextOwned {
    type Target = Context;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

pub struct ValueOwned {
    ctx: JSGlobalContextRef,
    /// Prevents the runtime `Rc` from dropping while this
    /// value is alive.
    _runtime: Option<Rc<crate::runtime::RuntimeInner>>,
    raw: JSValueRef,
}

impl ValueOwned {
    pub fn from_ref(ctx: &Context, value: &Value<'_>) -> Self {
        unsafe { JSValueProtect(ctx.as_ctx(), value.raw) };
        ValueOwned {
            ctx: ctx.raw(),
            _runtime: ctx._runtime.clone(),
            raw: value.raw,
        }
    }

    pub fn from_value(ctx: &Context, value: Value<'_>) -> Self {
        let raw = value.raw;
        mem::forget(value);
        ValueOwned { ctx: ctx.raw(), _runtime: ctx._runtime.clone(), raw }
    }

    /// Converts into a `Value` tied to the given context.
    ///
    /// # Safety
    /// The caller must ensure `ctx` points to the same
    /// underlying JSC context and outlives the returned
    /// value.
    pub fn into_value<'a>(self, ctx: &'a Context) -> Value<'a> {
        let value = unsafe { Value::from_raw(ctx, self.raw) };
        unsafe { JSValueUnprotect(self.ctx, self.raw) };
        mem::forget(self);
        value
    }
}

impl Drop for ValueOwned {
    fn drop(&mut self) {
        unsafe { JSValueUnprotect(self.ctx, self.raw) };
    }
}

pub struct ObjectOwned {
    ctx: JSGlobalContextRef,
    runtime: Option<Rc<crate::runtime::RuntimeInner>>,
    raw: JSObjectRef,
}

impl ObjectOwned {
    pub fn from_object(ctx: &Context, value: Object<'_>) -> Self {
        let raw = value.raw;
        mem::forget(value);
        ObjectOwned { ctx: ctx.raw(), runtime: ctx._runtime.clone(), raw }
    }

    /// Converts into an `Object` tied to the given context.
    ///
    /// # Safety
    /// The caller must ensure `ctx` points to the same
    /// underlying JSC context and outlives the returned
    /// object.
    pub fn into_object<'a>(self, ctx: &'a Context) -> Object<'a> {
        let obj = unsafe { Object::from_raw(ctx, self.raw) };
        unsafe { JSValueUnprotect(self.ctx, self.raw) };
        mem::forget(self);
        obj
    }

    pub fn into_value(self) -> ValueOwned {
        let value = ValueOwned {
            ctx: self.ctx,
            _runtime: self.runtime.clone(),
            raw: self.raw as JSValueRef,
        };
        mem::forget(self);
        value
    }
}

impl Drop for ObjectOwned {
    fn drop(&mut self) {
        unsafe { JSValueUnprotect(self.ctx, self.raw) };
    }
}

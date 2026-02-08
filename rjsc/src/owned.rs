use std::mem;
use std::rc::Rc;

use rjsc_sys::*;

use crate::{Context, Value};

/// An owned value that keeps the underlying JSC value protected.
///
/// This is useful for storing values across context boundaries.
pub struct ValueOwned {
    ctx: JSGlobalContextRef,
    /// Prevents the runtime `Rc` from dropping while this
    /// value is alive.
    _runtime: Option<Rc<crate::runtime::RuntimeInner>>,
    raw: JSValueRef,
}

impl ValueOwned {
    /// Creates an owned value from a reference.
    pub fn from_ref(ctx: &Context, value: &Value<'_>) -> Self {
        unsafe { JSValueProtect(ctx.as_ctx(), value.raw) };
        ValueOwned {
            ctx: ctx.raw(),
            _runtime: ctx._runtime.clone(),
            raw: value.raw,
        }
    }

    /// Creates an owned value from a value, transferring ownership.
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

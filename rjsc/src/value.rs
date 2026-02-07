use std::marker::PhantomData;
use std::ptr;

use rjsc_sys::*;

use crate::{js_string_to_rust, JsContext, JsException, JsObject};

/// A JavaScript value, tied to the lifetime of its [`JsContext`].
///
/// The value is protected from garbage collection while this handle exists.
pub struct JsValue<'ctx> {
    pub(crate) raw: JSValueRef,
    ctx: JSContextRef,
    _marker: PhantomData<&'ctx JsContext>,
}

impl<'ctx> JsValue<'ctx> {
    /// Wraps a raw `JSValueRef`, protecting it from GC.
    ///
    /// # Safety
    /// `raw` must be a valid `JSValueRef` belonging to the given context.
    pub(crate) unsafe fn from_raw(
        ctx: &'ctx JsContext,
        raw: JSValueRef,
    ) -> Self {
        unsafe { JSValueProtect(ctx.as_ctx(), raw) };
        JsValue { raw, ctx: ctx.as_ctx(), _marker: PhantomData }
    }

    /// Creates the JavaScript `undefined` value.
    pub fn undefined(ctx: &'ctx JsContext) -> Self {
        let raw = unsafe { JSValueMakeUndefined(ctx.as_ctx()) };
        unsafe { Self::from_raw(ctx, raw) }
    }

    /// Creates the JavaScript `null` value.
    pub fn null(ctx: &'ctx JsContext) -> Self {
        let raw = unsafe { JSValueMakeNull(ctx.as_ctx()) };
        unsafe { Self::from_raw(ctx, raw) }
    }

    /// Creates a JavaScript boolean value.
    pub fn from_bool(ctx: &'ctx JsContext, value: bool) -> Self {
        let raw = unsafe { JSValueMakeBoolean(ctx.as_ctx(), value) };
        unsafe { Self::from_raw(ctx, raw) }
    }

    /// Creates a JavaScript number value.
    pub fn from_f64(ctx: &'ctx JsContext, value: f64) -> Self {
        let raw = unsafe { JSValueMakeNumber(ctx.as_ctx(), value) };
        unsafe { Self::from_raw(ctx, raw) }
    }

    /// Creates a JavaScript string value.
    pub fn from_str(ctx: &'ctx JsContext, value: &str) -> Self {
        let raw = unsafe {
            let js_str = crate::js_string_from_rust(value);
            let val = JSValueMakeString(ctx.as_ctx(), js_str);
            JSStringRelease(js_str);
            val
        };
        unsafe { Self::from_raw(ctx, raw) }
    }

    /// Returns `true` if this value is the JavaScript `undefined` value.
    pub fn is_undefined(&self) -> bool {
        unsafe { JSValueIsUndefined(self.ctx, self.raw) }
    }

    /// Returns `true` if this value is the JavaScript `null` value.
    pub fn is_null(&self) -> bool {
        unsafe { JSValueIsNull(self.ctx, self.raw) }
    }

    /// Returns `true` if this value is a boolean.
    pub fn is_boolean(&self) -> bool {
        unsafe { JSValueIsBoolean(self.ctx, self.raw) }
    }

    /// Returns `true` if this value is a number.
    pub fn is_number(&self) -> bool {
        unsafe { JSValueIsNumber(self.ctx, self.raw) }
    }

    /// Returns `true` if this value is a string.
    pub fn is_string(&self) -> bool {
        unsafe { JSValueIsString(self.ctx, self.raw) }
    }

    /// Returns `true` if this value is an object.
    pub fn is_object(&self) -> bool {
        unsafe { JSValueIsObject(self.ctx, self.raw) }
    }

    /// Returns `true` if this value is an array.
    pub fn is_array(&self) -> bool {
        unsafe { JSValueIsArray(self.ctx, self.raw) }
    }

    /// Converts the value to a boolean (using JavaScript truthiness rules).
    pub fn to_boolean(&self) -> bool {
        unsafe { JSValueToBoolean(self.ctx, self.raw) }
    }

    /// Converts the value to an `f64`. Returns `NaN` if conversion fails.
    pub fn to_number(&self) -> f64 {
        unsafe { JSValueToNumber(self.ctx, self.raw, ptr::null_mut()) }
    }

    /// Converts this value to a [`JsObject`].
    ///
    /// Fails if the value is not an object (or convertible to one).
    pub fn to_object(
        self,
        ctx: &'ctx JsContext,
    ) -> Result<JsObject<'ctx>, JsException> {
        let mut exception: JSValueRef = ptr::null();
        let obj =
            unsafe { JSValueToObject(self.ctx, self.raw, &mut exception) };
        if !exception.is_null() {
            return Err(JsException::from_jsvalue(self.ctx, exception));
        }
        Ok(unsafe { JsObject::from_raw(ctx, obj) })
    }

    /// Converts the value to a Rust `String` using JavaScript's `String()`
    /// conversion.
    ///
    /// Invalid surrogate pairs are replaced with the Unicode replacement
    /// character.
    pub fn to_string_lossy(&self) -> String {
        unsafe {
            let js_str =
                JSValueToStringCopy(self.ctx, self.raw, ptr::null_mut());
            let s = js_string_to_rust(js_str);
            JSStringRelease(js_str);
            s
        }
    }
}

impl Drop for JsValue<'_> {
    fn drop(&mut self) {
        unsafe { JSValueUnprotect(self.ctx, self.raw) };
    }
}

impl std::fmt::Debug for JsValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JsValue")
            .field("repr", &self.to_string_lossy())
            .finish()
    }
}

impl std::fmt::Display for JsValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_lossy())
    }
}

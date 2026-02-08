use std::fmt::Debug;
use std::ptr;

use rjsc_sys::*;

use crate::{
    js_string_from_rust, js_string_to_rust, JsContext, JsException, JsValue,
};

/// A JavaScript object, tied to the lifetime of its [`JsContext`].
///
/// This wraps a `JSObjectRef` and provides property access, array
/// indexing, and method calls.
pub struct JsObject<'ctx> {
    pub(crate) raw: JSObjectRef,
    pub(crate) ctx: &'ctx JsContext,
}

impl<'ctx> JsObject<'ctx> {
    /// Wraps a raw `JSObjectRef`, protecting it from GC.
    ///
    /// # Safety
    /// `raw` must be a valid `JSObjectRef` belonging to the given
    /// context.
    pub(crate) unsafe fn from_raw(
        ctx: &'ctx JsContext,
        raw: JSObjectRef,
    ) -> Self {
        unsafe { JSValueProtect(ctx.as_ctx(), raw) };
        JsObject { raw, ctx }
    }

    /// Creates a new, empty JavaScript object.
    pub fn new(ctx: &'ctx JsContext) -> Self {
        let raw = unsafe {
            JSObjectMake(ctx.as_ctx(), ptr::null_mut(), ptr::null_mut())
        };
        unsafe { Self::from_raw(ctx, raw) }
    }

    /// Returns the context this object belongs to.
    pub fn context(&self) -> &'ctx JsContext {
        self.ctx
    }

    /// Returns `true` if the object has the given property.
    pub fn has(&self, key: &str) -> bool {
        unsafe {
            let js_key = js_string_from_rust(key);
            let result =
                JSObjectHasProperty(self.ctx.as_ctx(), self.raw, js_key);
            JSStringRelease(js_key);
            result
        }
    }

    /// Gets a property by name, returning it as a [`JsValue`].
    ///
    /// Returns the `undefined` value if the property doesn't
    /// exist.
    pub fn get(&self, key: &str) -> Result<JsValue<'ctx>, JsException> {
        let mut exception: JSValueRef = ptr::null();
        let val = unsafe {
            let js_key = js_string_from_rust(key);
            let v = JSObjectGetProperty(
                self.ctx.as_ctx(),
                self.raw,
                js_key,
                &mut exception,
            );
            JSStringRelease(js_key);
            v
        };
        if !exception.is_null() {
            return Err(JsException::from_jsvalue(
                self.ctx.as_ctx(),
                exception,
            ));
        }
        Ok(unsafe { JsValue::from_raw(self.ctx, val) })
    }

    /// Sets a property by name.
    pub fn set(
        &self,
        key: &str,
        value: &JsValue<'ctx>,
    ) -> Result<(), JsException> {
        let mut exception: JSValueRef = ptr::null();
        unsafe {
            let js_key = js_string_from_rust(key);
            JSObjectSetProperty(
                self.ctx.as_ctx(),
                self.raw,
                js_key,
                value.raw,
                0,
                &mut exception,
            );
            JSStringRelease(js_key);
        }
        if !exception.is_null() {
            return Err(JsException::from_jsvalue(
                self.ctx.as_ctx(),
                exception,
            ));
        }
        Ok(())
    }

    /// Deletes a property by name. Returns `true` if the property
    /// was deleted.
    pub fn delete(&self, key: &str) -> Result<bool, JsException> {
        let mut exception: JSValueRef = ptr::null();
        let result = unsafe {
            let js_key = js_string_from_rust(key);
            let r = JSObjectDeleteProperty(
                self.ctx.as_ctx(),
                self.raw,
                js_key,
                &mut exception,
            );
            JSStringRelease(js_key);
            r
        };
        if !exception.is_null() {
            return Err(JsException::from_jsvalue(
                self.ctx.as_ctx(),
                exception,
            ));
        }
        Ok(result)
    }

    /// Gets an element by numeric index (e.g. for arrays).
    pub fn get_index(&self, index: u32) -> Result<JsValue<'ctx>, JsException> {
        let mut exception: JSValueRef = ptr::null();
        let val = unsafe {
            JSObjectGetPropertyAtIndex(
                self.ctx.as_ctx(),
                self.raw,
                index,
                &mut exception,
            )
        };
        if !exception.is_null() {
            return Err(JsException::from_jsvalue(
                self.ctx.as_ctx(),
                exception,
            ));
        }
        Ok(unsafe { JsValue::from_raw(self.ctx, val) })
    }

    /// Sets an element by numeric index.
    pub fn set_index(
        &self,
        index: u32,
        value: &JsValue<'ctx>,
    ) -> Result<(), JsException> {
        let mut exception: JSValueRef = ptr::null();
        unsafe {
            JSObjectSetPropertyAtIndex(
                self.ctx.as_ctx(),
                self.raw,
                index,
                value.raw,
                &mut exception,
            );
        }
        if !exception.is_null() {
            return Err(JsException::from_jsvalue(
                self.ctx.as_ctx(),
                exception,
            ));
        }
        Ok(())
    }

    /// Returns the names of the object's enumerable properties.
    pub fn property_names(&self) -> Vec<String> {
        unsafe {
            let names = JSObjectCopyPropertyNames(self.ctx.as_ctx(), self.raw);
            let count = JSPropertyNameArrayGetCount(names);
            let mut result = Vec::with_capacity(count);
            for i in 0..count {
                let name = JSPropertyNameArrayGetNameAtIndex(names, i);
                result.push(js_string_to_rust(name));
            }
            JSPropertyNameArrayRelease(names);
            result
        }
    }

    /// Returns `true` if this object is callable as a function.
    pub fn is_function(&self) -> bool {
        unsafe { JSObjectIsFunction(self.ctx.as_ctx(), self.raw) }
    }

    /// Calls this object as a function with the given arguments.
    ///
    /// `this_obj` is the `this` binding for the call; pass `None`
    /// to use the global object.
    pub fn call(
        &self,
        this_obj: Option<&JsObject<'ctx>>,
        args: &[&JsValue<'ctx>],
    ) -> Result<JsValue<'ctx>, JsException> {
        let mut exception: JSValueRef = ptr::null();
        let raw_args: Vec<JSValueRef> = args.iter().map(|a| a.raw).collect();
        let this_raw = this_obj.map_or(ptr::null_mut(), |o| o.raw);
        let result = unsafe {
            JSObjectCallAsFunction(
                self.ctx.as_ctx(),
                self.raw,
                this_raw,
                raw_args.len(),
                if raw_args.is_empty() {
                    ptr::null()
                } else {
                    raw_args.as_ptr()
                },
                &mut exception,
            )
        };
        if !exception.is_null() {
            return Err(JsException::from_jsvalue(
                self.ctx.as_ctx(),
                exception,
            ));
        }
        Ok(unsafe { JsValue::from_raw(self.ctx, result) })
    }

    /// Gets a property and calls it as a method on this object.
    pub fn call_method(
        &self,
        method: &str,
        args: &[&JsValue<'ctx>],
    ) -> Result<JsValue<'ctx>, JsException> {
        let func_val = self.get(method)?;
        let func_obj = func_val.to_object()?;
        func_obj.call(Some(self), args)
    }

    /// Returns the raw `JSObjectRef`.
    pub fn raw(&self) -> JSObjectRef {
        self.raw
    }

    /// Converts this object back to a [`JsValue`].
    pub fn to_value(&self) -> JsValue<'ctx> {
        unsafe { JsValue::from_raw(self.ctx, self.raw) }
    }
}

impl Drop for JsObject<'_> {
    fn drop(&mut self) {
        unsafe { JSValueUnprotect(self.ctx.as_ctx(), self.raw) };
    }
}

impl Debug for JsObject<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JsObject")
            .field("properties", &self.property_names())
            .finish()
    }
}

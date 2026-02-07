use std::ffi::CString;
use std::ptr;
use std::rc::Rc;

use rjsc_sys::*;

use crate::callbacks::CallbackHolder;
use crate::callbacks::RawCb;
use crate::callbacks::rust_callback_finalize;
use crate::callbacks::rust_callback_trampoline;
use crate::callbacks::raw_callback_finalize;
use crate::callbacks::raw_callback_trampoline;
use crate::callbacks::RustCallback;
use crate::js_string_from_rust;
use crate::JsException;
use crate::JsPromise;
use crate::JsRuntime;
use crate::JsValue;

/// A JavaScript global execution context.
///
/// This owns the underlying JSC global context and releases it on drop.
pub struct JsContext {
    pub(crate) raw: JSGlobalContextRef,
    pub(crate) _not_send_sync: std::marker::PhantomData<Rc<()>>,
    pub(crate) _runtime: Option<Rc<crate::runtime::JsRuntimeInner>>,
}

impl JsContext {
    /// Creates a new JavaScript execution context in the given runtime.
    pub fn new_in(runtime: &JsRuntime) -> Self {
        let raw = unsafe {
            JSGlobalContextCreateInGroup(runtime.raw(), ptr::null_mut())
        };
        Self {
            raw,
            _not_send_sync: std::marker::PhantomData,
            _runtime: Some(runtime.inner_clone()),
        }
    }

    /// Evaluates a string of JavaScript and returns the result.
    pub fn eval(&self, code: &str) -> Result<JsValue<'_>, JsException> {
        let script = unsafe { js_string_from_rust(code) };
        let mut exception: JSValueRef = ptr::null();

        let result = unsafe {
            JSEvaluateScript(
                self.raw,
                script,
                ptr::null_mut(),
                ptr::null_mut(),
                1,
                &mut exception,
            )
        };
        unsafe { JSStringRelease(script) };

        if !exception.is_null() {
            return Err(JsException::from_jsvalue(self.raw, exception));
        }

        Ok(unsafe { JsValue::from_raw(self, result) })
    }

    /// Evaluates JavaScript code and, if it returns a promise, resolves it.
    ///
    /// This is a blocking helper that uses the default microtask drain driver.
    /// Use [`JsPromise::into_future`] for non-blocking integration.
    pub fn eval_async(&self, code: &str) -> Result<JsValue<'_>, JsException> {
        let value = self.eval(code)?;
        if !value.is_object() {
            return Ok(value);
        }

        let raw = value.raw;
        match JsPromise::from_value(self, value) {
            Ok(promise) => promise.await_blocking(self),
            Err(_) => Ok(unsafe { JsValue::from_raw(self, raw) }),
        }
    }

    /// Evaluates JavaScript code expected to return a promise.
    pub fn eval_promise(
        &self,
        code: &str,
    ) -> Result<JsPromise<'_>, JsException> {
        let value = self.eval(code)?;
        JsPromise::from_value(self, value)
    }

    /// Registers a Rust function as a global JavaScript function.
    ///
    /// The callback receives a slice of [`JsValue`] arguments and returns
    /// either a [`JsValue`] result or an error string.
    ///
    /// The closure must live at least as long as this `JsContext`.
    pub fn register_fn<F>(&self, name: &str, f: F)
    where
        F: for<'a> Fn(
                &'a JsContext,
                &[JsValue<'a>],
            ) -> Result<JsValue<'a>, String>
            + 'static,
    {
        let cb: Box<RustCallback> = Box::new(f);
        let holder = Box::new(CallbackHolder { cb });
        let private = Box::into_raw(holder) as *mut std::ffi::c_void;

        let mut def = unsafe { kJSClassDefinitionEmpty };
        let class_name = CString::new("RustFunction").unwrap();
        def.className = class_name.as_ptr();
        def.callAsFunction = Some(rust_callback_trampoline);
        def.finalize = Some(rust_callback_finalize);
        let class = unsafe { JSClassCreate(&def) };

        let func_obj = unsafe { JSObjectMake(self.raw, class, private) };
        unsafe { JSClassRelease(class) };

        unsafe {
            let global = JSContextGetGlobalObject(self.raw);
            let js_name = js_string_from_rust(name);
            JSObjectSetProperty(
                self.raw,
                global,
                js_name,
                func_obj,
                0,
                ptr::null_mut(),
            );
            JSStringRelease(js_name);
        }
    }

    /// Creates a raw JS function object from a Rust closure.
    ///
    /// The closure receives the raw context and a pointer to the first argument
    /// (may be null if argc == 0). This is an internal helper for async
    /// bridging and similar low-level uses.
    pub(crate) fn make_callback<F>(&self, f: F) -> JSObjectRef
    where
        F: Fn(JSContextRef, *const JSValueRef) -> JSValueRef + 'static,
    {
        let cb: Box<RawCb> = Box::new(f);
        let holder = Box::new(CallbackHolder { cb });
        let private = Box::into_raw(holder) as *mut std::ffi::c_void;

        let mut def = unsafe { kJSClassDefinitionEmpty };
        let name = CString::new("RjscRawCb").unwrap();
        def.className = name.as_ptr();
        def.callAsFunction = Some(raw_callback_trampoline);
        def.finalize = Some(raw_callback_finalize);
        let class = unsafe { JSClassCreate(&def) };
        let obj = unsafe { JSObjectMake(self.raw, class, private) };
        unsafe { JSClassRelease(class) };
        obj
    }

    /// Returns the raw `JSGlobalContextRef`.
    pub fn raw(&self) -> JSGlobalContextRef {
        self.raw
    }

    /// Returns the raw context as a `JSContextRef`.
    pub(crate) fn as_ctx(&self) -> JSContextRef {
        self.raw
    }
}

impl Drop for JsContext {
    fn drop(&mut self) {
        unsafe { JSGlobalContextRelease(self.raw) };
    }
}

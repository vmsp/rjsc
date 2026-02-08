use std::ffi::CString;
use std::ptr;
use std::rc::Rc;

use rjsc_sys::*;

use crate::callbacks::{
    raw_callback_finalize, raw_callback_trampoline, rust_callback_finalize,
    rust_callback_trampoline, CallbackHolder, RawCb, RustCallback,
};
use crate::js_string_from_rust;
use crate::{Exception, Object, Promise, Runtime, Value};

/// A JavaScript global execution context.
///
/// This owns the underlying JSC global context and releases it on drop.
pub struct Context {
    pub(crate) raw: JSGlobalContextRef,
    pub(crate) _not_send_sync: std::marker::PhantomData<Rc<()>>,
    pub(crate) _runtime: Option<Rc<crate::runtime::RuntimeInner>>,
}

impl Context {
    /// Creates a new JavaScript execution context in the given runtime.
    pub fn new_in(runtime: &Runtime) -> Self {
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
    pub fn eval(&self, code: &str) -> Result<Value<'_>, Exception> {
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
            return Err(Exception::from_jsvalue(self.raw, exception));
        }

        Ok(unsafe { Value::from_raw(self, result) })
    }

    /// Evaluates JavaScript code expected to return a promise.
    pub fn eval_promise(&self, code: &str) -> Result<Promise<'_>, Exception> {
        let value = self.eval(code)?;
        Promise::from_value(value)
    }

    /// Returns the global object.
    pub fn global(&self) -> Object<'_> {
        let raw = unsafe { JSContextGetGlobalObject(self.raw) };
        unsafe { Object::from_raw(self, raw) }
    }

    /// Creates a JavaScript function from a Rust closure.
    ///
    /// The callback receives a slice of [`Value`] arguments and returns
    /// either a [`Value`] result or an error string.
    ///
    /// The closure must live at least as long as this `Context`.
    pub fn create_function<F>(&self, f: F) -> Value<'_>
    where
        F: for<'a> Fn(&'a Context, &[Value<'a>]) -> Result<Value<'a>, String>
            + 'static,
    {
        let cb: Box<RustCallback> = Box::new(f);
        let holder =
            Box::new(CallbackHolder { cb, runtime: self._runtime.clone() });
        let private = Box::into_raw(holder) as *mut std::ffi::c_void;

        let mut def = unsafe { kJSClassDefinitionEmpty };
        let class_name = CString::new("RustFunction").unwrap();
        def.className = class_name.as_ptr();
        def.callAsFunction = Some(rust_callback_trampoline);
        def.finalize = Some(rust_callback_finalize);
        let class = unsafe { JSClassCreate(&def) };

        let func_obj = unsafe { JSObjectMake(self.raw, class, private) };
        unsafe { JSClassRelease(class) };
        unsafe { Value::from_raw(self, func_obj) }
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
        let holder =
            Box::new(CallbackHolder { cb, runtime: self._runtime.clone() });
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

    /// Access the reactor for this context's runtime.
    ///
    /// Returns None if this is a detached context without a runtime.
    pub(crate) fn with_reactor<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&dyn crate::reactor::Reactor) -> R,
    {
        self._runtime.as_ref().map(|rt| rt.with_reactor(f))
    }

    /// Notify the reactor that a promise has been settled.
    pub(crate) fn notify_reactor(&self) {
        self.with_reactor(|r| r.notify());
    }

    /// Get a reference to the runtime for this context.
    ///
    /// This is used by the #[function] macro for async functions.
    #[doc(hidden)]
    pub fn runtime(&self) -> Option<crate::Runtime> {
        self._runtime
            .as_ref()
            .map(|inner| crate::Runtime::from_inner(std::rc::Rc::clone(inner)))
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

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { JSGlobalContextRelease(self.raw) };
    }
}

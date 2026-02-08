use std::ptr;
use std::rc::Rc;

use rjsc_sys::*;

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
    pub fn new(runtime: &Runtime) -> Self {
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

    /// Get a reference to the runtime for this context.
    ///
    /// Returns `None` if the context is detached and has no associated runtime.
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

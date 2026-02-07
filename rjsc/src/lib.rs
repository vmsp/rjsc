mod object;
mod promise;
mod value;

use std::ffi::CStr;
use std::ffi::CString;
use std::fmt;
use std::mem;
use std::mem::ManuallyDrop;
use std::ptr;
use std::rc::Rc;

use rjsc_sys::*;

pub use object::JsObject;
pub use promise::{
    JsJobDriver, JsMicrotaskDrain, JsPromise, JsPromiseFuture,
    JsPromiseResolver,
};
pub use value::JsValue;

/// A JavaScript runtime (context group).
///
/// Contexts created from the same runtime share a VM and must remain on the
/// same thread.
pub struct JsRuntime {
    inner: Rc<JsRuntimeInner>,
}

impl JsRuntime {
    /// Creates a new JavaScript runtime (context group).
    pub fn new() -> Self {
        let raw = unsafe { JSContextGroupCreate() };
        JsRuntime { inner: Rc::new(JsRuntimeInner { raw }) }
    }

    /// Creates a new context inside this runtime.
    pub fn new_context(&self) -> JsContext {
        JsContext::new_in(self)
    }
}

impl Default for JsRuntime {
    fn default() -> Self {
        Self::new()
    }
}

struct JsRuntimeInner {
    raw: JSContextGroupRef,
}

impl Drop for JsRuntimeInner {
    fn drop(&mut self) {
        unsafe { JSContextGroupRelease(self.raw) };
    }
}

/// A JavaScript global execution context.
///
/// This owns the underlying JSC global context and releases it on drop.
pub struct JsContext {
    raw: JSGlobalContextRef,
    _not_send_sync: std::marker::PhantomData<Rc<()>>,
    _runtime: Option<Rc<JsRuntimeInner>>,
}

impl JsContext {
    /// Creates a new JavaScript execution context in the given runtime.
    pub fn new_in(runtime: &JsRuntime) -> Self {
        let raw = unsafe {
            JSGlobalContextCreateInGroup(runtime.inner.raw, ptr::null_mut())
        };
        Self {
            raw,
            _not_send_sync: std::marker::PhantomData,
            _runtime: Some(Rc::clone(&runtime.inner)),
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

        // Create a JSClass with callAsFunction + finalize.
        let mut def = unsafe { kJSClassDefinitionEmpty };
        let class_name = CString::new("RustFunction").unwrap();
        def.className = class_name.as_ptr();
        def.callAsFunction = Some(rust_callback_trampoline);
        def.finalize = Some(rust_callback_finalize);
        let class = unsafe { JSClassCreate(&def) };

        // Make the function object with our private data.
        let func_obj = unsafe { JSObjectMake(self.raw, class, private) };
        unsafe { JSClassRelease(class) };

        // Set it as a property on the global object.
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

/// A JavaScript execution error.
#[derive(Debug)]
pub struct JsException {
    message: String,
}

impl JsException {
    pub(crate) fn from_jsvalue(ctx: JSContextRef, val: JSValueRef) -> Self {
        let message = unsafe {
            let js_str = JSValueToStringCopy(ctx, val, ptr::null_mut());
            let s = js_string_to_rust(js_str);
            JSStringRelease(js_str);
            s
        };
        JsException { message }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for JsException {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "JsException: {}", self.message)
    }
}

impl std::error::Error for JsException {}

type RustCallback = dyn for<'a> Fn(&'a JsContext, &[JsValue<'a>]) -> Result<JsValue<'a>, String>
    + 'static;

struct CallbackHolder<T: ?Sized> {
    cb: Box<T>,
}

unsafe extern "C" fn rust_callback_trampoline(
    ctx: JSContextRef,
    function: JSObjectRef,
    _this: JSObjectRef,
    argc: usize,
    argv: *const JSValueRef,
    exception: *mut JSValueRef,
) -> JSValueRef {
    let private = unsafe { JSObjectGetPrivate(function) };
    if private.is_null() {
        return unsafe { JSValueMakeUndefined(ctx) };
    }
    let holder = unsafe { &*(private as *const CallbackHolder<RustCallback>) };
    let cb = &*holder.cb;

    // Build a temporary JsContext wrapper. We must NOT drop this
    // (it doesn't own the context), so use ManuallyDrop.
    let temp_ctx = ManuallyDrop::new(JsContext {
        raw: ctx as JSGlobalContextRef,
        _not_send_sync: std::marker::PhantomData,
        _runtime: None,
    });

    // Wrap the arguments as JsValues. We protect them so they
    // survive for the duration of the callback, then unprotect.
    let args: Vec<JsValue<'_>> = (0..argc)
        .map(|i| {
            let raw = unsafe { *argv.add(i) };
            unsafe { JsValue::from_raw(&temp_ctx, raw) }
        })
        .collect();

    match cb(&temp_ctx, &args) {
        Ok(val) => {
            let raw = val.raw;
            // Prevent the JsValue from unprotecting on drop —
            // the caller (JSC) manages the return value.
            mem::forget(val);
            raw
        }
        Err(msg) => {
            // Set the exception.
            let js_msg = unsafe {
                let s = js_string_from_rust(&msg);
                let v = JSValueMakeString(ctx, s);
                JSStringRelease(s);
                v
            };
            unsafe {
                *exception = js_msg;
            }
            unsafe { JSValueMakeUndefined(ctx) }
        }
    }
}

unsafe extern "C" fn rust_callback_finalize(obj: JSObjectRef) {
    let private = unsafe { JSObjectGetPrivate(obj) };
    if !private.is_null() {
        // Drop the boxed closure.
        let _ = unsafe {
            Box::from_raw(private as *mut CallbackHolder<RustCallback>)
        };
    }
}

type RawCb = dyn Fn(JSContextRef, *const JSValueRef) -> JSValueRef + 'static;

unsafe extern "C" fn raw_callback_trampoline(
    ctx: JSContextRef,
    function: JSObjectRef,
    _this: JSObjectRef,
    argc: usize,
    argv: *const JSValueRef,
    _exception: *mut JSValueRef,
) -> JSValueRef {
    let private = unsafe { JSObjectGetPrivate(function) };
    if private.is_null() {
        return unsafe { JSValueMakeUndefined(ctx) };
    }
    let holder = unsafe { &*(private as *const CallbackHolder<RawCb>) };
    let cb = &*holder.cb;
    let args = if argc == 0 { ptr::null() } else { argv };
    cb(ctx, args)
}

unsafe extern "C" fn raw_callback_finalize(obj: JSObjectRef) {
    let private = unsafe { JSObjectGetPrivate(obj) };
    if !private.is_null() {
        let _ = unsafe { Box::from_raw(private as *mut CallbackHolder<RawCb>) };
    }
}

unsafe fn js_string_from_rust(s: &str) -> JSStringRef {
    let c_string = CString::new(s).expect("JS string contained interior NUL");
    unsafe { JSStringCreateWithUTF8CString(c_string.as_ptr()) }
}

unsafe fn js_string_to_rust(js_str: JSStringRef) -> String {
    let max_len = unsafe { JSStringGetMaximumUTF8CStringSize(js_str) };
    let mut buf = vec![0u8; max_len];
    unsafe {
        JSStringGetUTF8CString(js_str, buf.as_mut_ptr() as *mut _, max_len)
    };
    let c_str = CStr::from_bytes_until_nul(&buf).unwrap();
    c_str.to_string_lossy().into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::task::noop_waker;
    use std::pin::Pin;
    use std::task::{Context, Poll};
    
    fn test_ctx() -> JsContext {
        let runtime = JsRuntime::new();
        JsContext::new_in(&runtime)
    }

    fn poll_promise_future<'ctx>(
        mut fut: Pin<&mut JsPromiseFuture<'ctx>>,
        ctx: &'ctx JsContext,
    ) -> Result<JsValue<'ctx>, JsException> {
        let waker = noop_waker();
        let mut cx = Context::from_waker(&waker);
        let driver = JsMicrotaskDrain::default();

        for _ in 0..driver.max_rounds() {
            match fut.as_mut().poll(&mut cx) {
                Poll::Ready(result) => return result,
                Poll::Pending => {
                    driver.drain_jobs(ctx)?;
                }
            }
        }

        Err(JsException {
            message: "Future did not resolve within drain budget.".to_string(),
        })
    }

    #[test]
    fn eval_arithmetic() {
        let ctx = test_ctx();
        let val = ctx.eval("1 + 2").unwrap();
        assert_eq!(val.to_number(), 3.0);
    }

    #[test]
    fn eval_string() {
        let ctx = test_ctx();
        let val = ctx.eval("'hello' + ' world'").unwrap();
        assert_eq!(val.to_string_lossy(), "hello world");
    }

    #[test]
    fn eval_boolean() {
        let ctx = test_ctx();
        let val = ctx.eval("true").unwrap();
        assert!(val.to_boolean());

        let val = ctx.eval("false").unwrap();
        assert!(!val.to_boolean());
    }

    #[test]
    fn eval_undefined_and_null() {
        let ctx = test_ctx();

        let val = ctx.eval("undefined").unwrap();
        assert!(val.is_undefined());

        let val = ctx.eval("null").unwrap();
        assert!(val.is_null());
    }

    #[test]
    fn eval_syntax_error() {
        let ctx = test_ctx();
        let err = ctx.eval("let x = ").unwrap_err();
        assert!(
            err.message().contains("SyntaxError"),
            "got: {}",
            err.message()
        );
    }

    #[test]
    fn eval_runtime_error() {
        let ctx = test_ctx();
        let err = ctx.eval("nonexistent()").unwrap_err();
        assert!(
            err.message().contains("ReferenceError"),
            "got: {}",
            err.message()
        );
    }

    #[test]
    fn eval_async_resolved() {
        let ctx = test_ctx();
        let val = ctx.eval_async("Promise.resolve(42)").unwrap();
        assert_eq!(val.to_number(), 42.0);
    }

    #[test]
    fn eval_async_function() {
        let ctx = test_ctx();
        let val = ctx
            .eval_async("(async () => { return 'async works'; })()")
            .unwrap();
        assert_eq!(val.to_string_lossy(), "async works");
    }

    #[test]
    fn eval_async_rejected() {
        let ctx = test_ctx();
        let err = ctx.eval_async("Promise.reject('boom')").unwrap_err();
        assert_eq!(err.message(), "boom");
    }

    #[test]
    fn eval_async_chain() {
        let ctx = test_ctx();
        let val = ctx
            .eval_async("Promise.resolve(10).then(x => x * 2).then(x => x + 1)")
            .unwrap();
        assert_eq!(val.to_number(), 21.0);
    }

    #[test]
    fn eval_async_non_promise() {
        let ctx = test_ctx();
        // If the code doesn't return a promise, eval_async returns the value.
        let val = ctx.eval_async("42").unwrap();
        assert_eq!(val.to_number(), 42.0);
    }

    #[test]
    fn eval_promise_returns_promise() {
        let ctx = test_ctx();
        let promise = ctx.eval_promise("Promise.resolve(7)").unwrap();
        let val = promise.await_blocking(&ctx).unwrap();
        assert_eq!(val.to_number(), 7.0);
    }

    #[test]
    fn value_to_promise() {
        let ctx = test_ctx();
        let val = ctx.eval("Promise.resolve(9)").unwrap();
        let promise = val.to_promise(&ctx).unwrap();
        let val = promise.await_blocking(&ctx).unwrap();
        assert_eq!(val.to_number(), 9.0);
    }

    #[test]
    fn promise_deferred_resolve() {
        let ctx = test_ctx();
        let (promise, resolver) = JsPromise::deferred(&ctx).unwrap();
        let mut fut = Box::pin(promise.into_future(&ctx));
        let waker = noop_waker();
        let mut cx = Context::from_waker(&waker);
        assert!(matches!(fut.as_mut().poll(&mut cx), Poll::Pending));
        let value = JsValue::from_str(&ctx, "done");
        resolver.resolve(&ctx, &value).unwrap();
        let val = poll_promise_future(fut.as_mut(), &ctx).unwrap();
        assert_eq!(val.to_string_lossy(), "done");
    }

    #[test]
    fn promise_deferred_reject() {
        let ctx = test_ctx();
        let (promise, resolver) = JsPromise::deferred(&ctx).unwrap();
        let mut fut = Box::pin(promise.into_future(&ctx));
        let waker = noop_waker();
        let mut cx = Context::from_waker(&waker);
        assert!(matches!(fut.as_mut().poll(&mut cx), Poll::Pending));
        resolver.reject_str(&ctx, "nope").unwrap();
        let err = poll_promise_future(fut.as_mut(), &ctx).unwrap_err();
        assert_eq!(err.message(), "nope");
    }

    #[test]
    fn promise_into_future() {
        let ctx = test_ctx();
        let promise = ctx.eval_promise("Promise.resolve(123)").unwrap();
        let mut fut = Box::pin(promise.into_future(&ctx));
        let val = poll_promise_future(fut.as_mut(), &ctx).unwrap();
        assert_eq!(val.to_number(), 123.0);
    }

    #[test]
    fn value_undefined() {
        let ctx = test_ctx();
        let val = JsValue::undefined(&ctx);
        assert!(val.is_undefined());
        assert!(!val.is_null());
        assert_eq!(val.to_string_lossy(), "undefined");
    }

    #[test]
    fn value_null() {
        let ctx = test_ctx();
        let val = JsValue::null(&ctx);
        assert!(val.is_null());
        assert!(!val.is_undefined());
        assert_eq!(val.to_string_lossy(), "null");
    }

    #[test]
    fn value_from_bool() {
        let ctx = test_ctx();
        let t = JsValue::from_bool(&ctx, true);
        assert!(t.is_boolean());
        assert!(t.to_boolean());

        let f = JsValue::from_bool(&ctx, false);
        assert!(f.is_boolean());
        assert!(!f.to_boolean());
    }

    #[test]
    fn value_from_f64() {
        let ctx = test_ctx();
        let val = JsValue::from_f64(&ctx, 3.14);
        assert!(val.is_number());
        assert!((val.to_number() - 3.14).abs() < f64::EPSILON);
    }

    #[test]
    fn value_from_str() {
        let ctx = test_ctx();
        let val = JsValue::from_str(&ctx, "hello from rust");
        assert!(val.is_string());
        assert_eq!(val.to_string_lossy(), "hello from rust");
    }

    #[test]
    fn object_new_and_properties() {
        let ctx = test_ctx();
        let obj = JsObject::new(&ctx);

        assert!(!obj.has("foo"));

        let val = JsValue::from_f64(&ctx, 42.0);
        obj.set("foo", &val).unwrap();
        assert!(obj.has("foo"));

        let got = obj.get("foo", &ctx).unwrap();
        assert_eq!(got.to_number(), 42.0);

        let names = obj.property_names();
        assert_eq!(names, vec!["foo"]);
    }

    #[test]
    fn object_delete_property() {
        let ctx = test_ctx();
        let obj = JsObject::new(&ctx);
        let val = JsValue::from_str(&ctx, "bar");
        obj.set("x", &val).unwrap();
        assert!(obj.has("x"));

        let deleted = obj.delete("x").unwrap();
        assert!(deleted);
        assert!(!obj.has("x"));
    }

    #[test]
    fn object_index_access() {
        let ctx = test_ctx();
        let arr = ctx.eval("[10, 20, 30]").unwrap();
        let arr_obj = arr.to_object(&ctx).unwrap();

        let first = arr_obj.get_index(0, &ctx).unwrap();
        assert_eq!(first.to_number(), 10.0);

        let third = arr_obj.get_index(2, &ctx).unwrap();
        assert_eq!(third.to_number(), 30.0);

        let replacement = JsValue::from_f64(&ctx, 99.0);
        arr_obj.set_index(1, &replacement).unwrap();
        let updated = arr_obj.get_index(1, &ctx).unwrap();
        assert_eq!(updated.to_number(), 99.0);
    }

    #[test]
    fn object_from_eval() {
        let ctx = test_ctx();
        let val = ctx.eval("({a: 1, b: 'two'})").unwrap();
        let obj = val.to_object(&ctx).unwrap();

        let a = obj.get("a", &ctx).unwrap();
        assert_eq!(a.to_number(), 1.0);

        let b = obj.get("b", &ctx).unwrap();
        assert_eq!(b.to_string_lossy(), "two");
    }

    #[test]
    fn object_call_function() {
        let ctx = test_ctx();
        let func_val = ctx.eval("(function(x) { return x * 2; })").unwrap();
        let func = func_val.to_object(&ctx).unwrap();

        assert!(func.is_function());

        let arg = JsValue::from_f64(&ctx, 21.0);
        let result = func.call(None, &[&arg], &ctx).unwrap();
        assert_eq!(result.to_number(), 42.0);
    }

    #[test]
    fn object_call_method() {
        let ctx = test_ctx();
        let val = ctx
            .eval("({value: 10, double() { return this.value * 2; }})")
            .unwrap();
        let obj = val.to_object(&ctx).unwrap();

        let result = obj.call_method("double", &[], &ctx).unwrap();
        assert_eq!(result.to_number(), 20.0);
    }

    #[test]
    fn register_fn_basic() {
        let ctx = test_ctx();
        ctx.register_fn("add", |ctx, args| {
            let a = args[0].to_number();
            let b = args[1].to_number();
            Ok(JsValue::from_f64(ctx, a + b))
        });
        let result = ctx.eval("add(3, 4)").unwrap();
        assert_eq!(result.to_number(), 7.0);
    }

    #[test]
    fn register_fn_returns_string() {
        let ctx = test_ctx();
        ctx.register_fn("greet", |ctx, args| {
            let name = args[0].to_string_lossy();
            Ok(JsValue::from_str(ctx, &format!("hi, {name}")))
        });
        let result = ctx.eval("greet('world')").unwrap();
        assert_eq!(result.to_string_lossy(), "hi, world");
    }

    #[test]
    fn register_fn_error() {
        let ctx = test_ctx();
        ctx.register_fn("fail", |_ctx, _args| {
            Err("something went wrong".into())
        });
        let err = ctx.eval("fail()").unwrap_err();
        assert!(
            err.message().contains("something went wrong"),
            "got: {}",
            err.message()
        );
    }

    #[test]
    fn register_fn_no_args() {
        let ctx = test_ctx();
        ctx.register_fn("forty_two", |ctx, _args| {
            Ok(JsValue::from_f64(ctx, 42.0))
        });
        let result = ctx.eval("forty_two()").unwrap();
        assert_eq!(result.to_number(), 42.0);
    }

    #[test]
    fn register_fn_called_from_js_function() {
        let ctx = test_ctx();
        ctx.register_fn("double", |ctx, args| {
            let n = args[0].to_number();
            Ok(JsValue::from_f64(ctx, n * 2.0))
        });
        let result = ctx.eval("[1,2,3].map(double)").unwrap();
        let arr = result.to_object(&ctx).unwrap();
        assert_eq!(arr.get_index(0, &ctx).unwrap().to_number(), 2.0);
        assert_eq!(arr.get_index(1, &ctx).unwrap().to_number(), 4.0);
        assert_eq!(arr.get_index(2, &ctx).unwrap().to_number(), 6.0);
    }
}

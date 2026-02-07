mod object;
mod value;

use std::cell::Cell;
use std::ffi::CStr;
use std::fmt;
use std::ptr;
use std::rc::Rc;

use rjsc_sys::*;

pub use object::JsObject;
pub use value::JsValue;

/// A JavaScript global execution context.
///
/// This owns the underlying JSC global context and releases it on drop.
pub struct JsContext {
    raw: JSGlobalContextRef,
}

impl JsContext {
    /// Creates a new JavaScript execution context with the default global
    /// object.
    pub fn new() -> Self {
        let raw = unsafe { JSGlobalContextCreate(ptr::null_mut()) };
        JsContext { raw }
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

    /// Evaluates JavaScript code that returns a promise and resolves it.
    ///
    /// The code is expected to produce a promise (e.g. an async function call).
    /// The promise is resolved (or rejected) via `.then()` / `.catch()`, and
    /// JSC's automatic microtask draining handles the resolution before control
    /// returns to Rust.
    ///
    /// Returns the resolved value on success, or the rejection reason as an
    /// error.
    pub fn eval_async(&self, code: &str) -> Result<JsValue<'_>, JsException> {
        let promise = self.eval(code)?;

        if !promise.is_object() {
            return Ok(promise);
        }

        struct AsyncState {
            settled: Cell<bool>,
            resolved: Cell<JSValueRef>,
            rejected: Cell<JSValueRef>,
        }

        let state = Rc::new(AsyncState {
            settled: Cell::new(false),
            resolved: Cell::new(ptr::null()),
            rejected: Cell::new(ptr::null()),
        });

        // Build on_resolve callback.
        let s = Rc::clone(&state);
        let on_resolve = self.make_callback(move |ctx, args| {
            let val = if args.is_null() {
                unsafe { JSValueMakeUndefined(ctx) }
            } else {
                unsafe { *args }
            };
            unsafe { JSValueProtect(ctx, val) };
            s.resolved.set(val);
            s.settled.set(true);
            unsafe { JSValueMakeUndefined(ctx) }
        });

        let s = Rc::clone(&state);
        let on_reject = self.make_callback(move |ctx, args| {
            let val = if args.is_null() {
                unsafe { JSValueMakeUndefined(ctx) }
            } else {
                unsafe { *args }
            };
            unsafe { JSValueProtect(ctx, val) };
            s.rejected.set(val);
            s.settled.set(true);
            unsafe { JSValueMakeUndefined(ctx) }
        });

        // Get the `then` method from the promise object.
        let promise_obj =
            unsafe { JSValueToObject(self.raw, promise.raw, ptr::null_mut()) };
        let then_key = unsafe { js_string_from_rust("then") };
        let then_fn = unsafe {
            let val = JSObjectGetProperty(
                self.raw,
                promise_obj,
                then_key,
                ptr::null_mut(),
            );
            JSStringRelease(then_key);
            JSValueToObject(self.raw, val, ptr::null_mut())
        };

        // Call promise.then(onResolve, onReject). This enqueues
        // microtasks; JSC drains them when the C API call returns.
        let args = [on_resolve as JSValueRef, on_reject as JSValueRef];
        unsafe {
            JSObjectCallAsFunction(
                self.raw,
                then_fn,
                promise_obj,
                2,
                args.as_ptr(),
                ptr::null_mut(),
            );
        }

        // Trigger microtask draining. JSC drains the microtask
        // queue when JSEvaluateScript returns. Chained promises
        // may need multiple drain rounds, so we loop.
        let noop = unsafe { js_string_from_rust("0") };
        for _ in 0..64 {
            if state.settled.get() {
                break;
            }
            unsafe {
                JSEvaluateScript(
                    self.raw,
                    noop,
                    ptr::null_mut(),
                    ptr::null_mut(),
                    0,
                    ptr::null_mut(),
                );
            }
        }
        unsafe { JSStringRelease(noop) };

        if !state.settled.get() {
            return Err(JsException {
                message: "Promise did not settle synchronously. \
                    eval_async only supports promises that resolve \
                    via microtasks (no pending I/O or timers)."
                    .to_string(),
            });
        }

        let rejected_raw = state.rejected.get();
        if !rejected_raw.is_null() {
            let err = JsException::from_jsvalue(self.raw, rejected_raw);
            unsafe { JSValueUnprotect(self.raw, rejected_raw) };
            return Err(err);
        }

        let resolved_raw = state.resolved.get();
        // Wrap into JsValue (which will protect it again), then
        // release our extra protect.
        let result = unsafe { JsValue::from_raw(self, resolved_raw) };
        unsafe { JSValueUnprotect(self.raw, resolved_raw) };
        Ok(result)
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
        // Double-box: the inner Box<dyn Fn> is a fat pointer,
        // so we wrap it in another Box to get a thin pointer
        // that can round-trip through *mut c_void.
        let inner: Box<RustCallback> = Box::new(f);
        let outer: Box<Box<RustCallback>> = Box::new(inner);
        let private = Box::into_raw(outer) as *mut std::ffi::c_void;

        // Create a JSClass with callAsFunction + finalize.
        let mut def = unsafe { kJSClassDefinitionEmpty };
        let class_name = std::ffi::CString::new("RustFunction").unwrap();
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
    /// (may be null if argc == 0). This is an internal helper for `eval_async`
    /// and similar low-level uses.
    fn make_callback<F>(&self, f: F) -> JSObjectRef
    where
        F: Fn(JSContextRef, *const JSValueRef) -> JSValueRef + 'static,
    {
        type RawCb =
            dyn Fn(JSContextRef, *const JSValueRef) -> JSValueRef + 'static;

        let inner: Box<RawCb> = Box::new(f);
        let outer: Box<Box<RawCb>> = Box::new(inner);
        let private = Box::into_raw(outer) as *mut std::ffi::c_void;

        let mut def = unsafe { kJSClassDefinitionEmpty };
        let name = std::ffi::CString::new("RjscRawCb").unwrap();
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

impl Default for JsContext {
    fn default() -> Self {
        Self::new()
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

// -- Callback bridging --

type RustCallback = dyn for<'a> Fn(&'a JsContext, &[JsValue<'a>]) -> Result<JsValue<'a>, String>
    + 'static;

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
    let cb = unsafe { &**(private as *const Box<RustCallback>) };

    // Build a temporary JsContext wrapper. We must NOT drop this
    // (it doesn't own the context), so use ManuallyDrop.
    let temp_ctx = std::mem::ManuallyDrop::new(JsContext {
        raw: ctx as JSGlobalContextRef,
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
            std::mem::forget(val);
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
        let _ = unsafe { Box::from_raw(private as *mut Box<RustCallback>) };
    }
}

// -- Raw callback bridging (for make_callback) --

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
    let cb = unsafe { &**(private as *const Box<RawCb>) };
    let args = if argc == 0 { ptr::null() } else { argv };
    cb(ctx, args)
}

unsafe extern "C" fn raw_callback_finalize(obj: JSObjectRef) {
    let private = unsafe { JSObjectGetPrivate(obj) };
    if !private.is_null() {
        let _ = unsafe { Box::from_raw(private as *mut Box<RawCb>) };
    }
}

// -- Internal helpers --

unsafe fn js_string_from_rust(s: &str) -> JSStringRef {
    // We need a null-terminated string. Use a small stack buffer when possible.
    let c_string =
        std::ffi::CString::new(s).expect("JS string contained interior NUL");
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
    use std::sync::Mutex;

    #[test]
    fn eval_arithmetic() {
        let ctx = JsContext::new();
        let val = ctx.eval("1 + 2").unwrap();
        assert_eq!(val.to_number(), 3.0);
    }

    #[test]
    fn eval_string() {
        let ctx = JsContext::new();
        let val = ctx.eval("'hello' + ' world'").unwrap();
        assert_eq!(val.to_string_lossy(), "hello world");
    }

    #[test]
    fn eval_boolean() {
        let ctx = JsContext::new();
        let val = ctx.eval("true").unwrap();
        assert!(val.to_boolean());

        let val = ctx.eval("false").unwrap();
        assert!(!val.to_boolean());
    }

    #[test]
    fn eval_undefined_and_null() {
        let ctx = JsContext::new();

        let val = ctx.eval("undefined").unwrap();
        assert!(val.is_undefined());

        let val = ctx.eval("null").unwrap();
        assert!(val.is_null());
    }

    #[test]
    fn eval_syntax_error() {
        let ctx = JsContext::new();
        let err = ctx.eval("let x = ").unwrap_err();
        assert!(
            err.message().contains("SyntaxError"),
            "got: {}",
            err.message()
        );
    }

    #[test]
    fn eval_runtime_error() {
        let ctx = JsContext::new();
        let err = ctx.eval("nonexistent()").unwrap_err();
        assert!(
            err.message().contains("ReferenceError"),
            "got: {}",
            err.message()
        );
    }

    #[test]
    fn eval_async_resolved() {
        let ctx = JsContext::new();
        let val = ctx.eval_async("Promise.resolve(42)").unwrap();
        assert_eq!(val.to_number(), 42.0);
    }

    #[test]
    fn eval_async_function() {
        let ctx = JsContext::new();
        let val = ctx
            .eval_async("(async () => { return 'async works'; })()")
            .unwrap();
        assert_eq!(val.to_string_lossy(), "async works");
    }

    #[test]
    fn eval_async_rejected() {
        let ctx = JsContext::new();
        let err = ctx.eval_async("Promise.reject('boom')").unwrap_err();
        assert_eq!(err.message(), "boom");
    }

    #[test]
    fn eval_async_chain() {
        let ctx = JsContext::new();
        let val = ctx
            .eval_async("Promise.resolve(10).then(x => x * 2).then(x => x + 1)")
            .unwrap();
        assert_eq!(val.to_number(), 21.0);
    }

    #[test]
    fn eval_async_non_promise() {
        let ctx = JsContext::new();
        // If the code doesn't return a promise, eval_async just returns the value.
        let val = ctx.eval_async("42").unwrap();
        assert_eq!(val.to_number(), 42.0);
    }

    #[test]
    fn value_undefined() {
        let ctx = JsContext::new();
        let val = JsValue::undefined(&ctx);
        assert!(val.is_undefined());
        assert!(!val.is_null());
        assert_eq!(val.to_string_lossy(), "undefined");
    }

    #[test]
    fn value_null() {
        let ctx = JsContext::new();
        let val = JsValue::null(&ctx);
        assert!(val.is_null());
        assert!(!val.is_undefined());
        assert_eq!(val.to_string_lossy(), "null");
    }

    #[test]
    fn value_from_bool() {
        let ctx = JsContext::new();
        let t = JsValue::from_bool(&ctx, true);
        assert!(t.is_boolean());
        assert!(t.to_boolean());

        let f = JsValue::from_bool(&ctx, false);
        assert!(f.is_boolean());
        assert!(!f.to_boolean());
    }

    #[test]
    fn value_from_f64() {
        let ctx = JsContext::new();
        let val = JsValue::from_f64(&ctx, 3.14);
        assert!(val.is_number());
        assert!((val.to_number() - 3.14).abs() < f64::EPSILON);
    }

    #[test]
    fn value_from_str() {
        let ctx = JsContext::new();
        let val = JsValue::from_str(&ctx, "hello from rust");
        assert!(val.is_string());
        assert_eq!(val.to_string_lossy(), "hello from rust");
    }

    #[test]
    fn object_new_and_properties() {
        let ctx = JsContext::new();
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
        let ctx = JsContext::new();
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
        let ctx = JsContext::new();
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
        let ctx = JsContext::new();
        let val = ctx.eval("({a: 1, b: 'two'})").unwrap();
        let obj = val.to_object(&ctx).unwrap();

        let a = obj.get("a", &ctx).unwrap();
        assert_eq!(a.to_number(), 1.0);

        let b = obj.get("b", &ctx).unwrap();
        assert_eq!(b.to_string_lossy(), "two");
    }

    #[test]
    fn object_call_function() {
        let ctx = JsContext::new();
        let func_val = ctx.eval("(function(x) { return x * 2; })").unwrap();
        let func = func_val.to_object(&ctx).unwrap();

        assert!(func.is_function());

        let arg = JsValue::from_f64(&ctx, 21.0);
        let result = func.call(None, &[&arg], &ctx).unwrap();
        assert_eq!(result.to_number(), 42.0);
    }

    #[test]
    fn object_call_method() {
        let ctx = JsContext::new();
        let val = ctx
            .eval("({value: 10, double() { return this.value * 2; }})")
            .unwrap();
        let obj = val.to_object(&ctx).unwrap();

        let result = obj.call_method("double", &[], &ctx).unwrap();
        assert_eq!(result.to_number(), 20.0);
    }

    #[test]
    fn register_fn_basic() {
        let ctx = JsContext::new();
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
        let ctx = JsContext::new();
        ctx.register_fn("greet", |ctx, args| {
            let name = args[0].to_string_lossy();
            Ok(JsValue::from_str(ctx, &format!("hi, {name}")))
        });
        let result = ctx.eval("greet('world')").unwrap();
        assert_eq!(result.to_string_lossy(), "hi, world");
    }

    #[test]
    fn register_fn_error() {
        let ctx = JsContext::new();
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
        let ctx = JsContext::new();
        ctx.register_fn("forty_two", |ctx, _args| {
            Ok(JsValue::from_f64(ctx, 42.0))
        });
        let result = ctx.eval("forty_two()").unwrap();
        assert_eq!(result.to_number(), 42.0);
    }

    #[test]
    fn register_fn_called_from_js_function() {
        let ctx = JsContext::new();
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

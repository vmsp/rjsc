//! JavaScript Function creation and management.

use std::ffi::CString;

use rjsc_sys::*;

use crate::callbacks::{rust_callback_finalize, rust_callback_trampoline};
use crate::{Context, Value};

// Re-export callback types for use in this module
use crate::callbacks::{CallbackHolder, RawCb, RustCallback};

/// A JavaScript function.
///
/// This type provides methods for creating JavaScript functions
/// from Rust closures.
pub struct Function;

impl Function {
    /// Creates a JavaScript function from a Rust closure.
    ///
    /// The callback receives a slice of [`Value`] arguments and returns
    /// either a [`Value`] result or an error string.
    ///
    /// The closure must live at least as long as the [`Context`].
    ///
    /// # Example
    ///
    /// ```
    /// use rjsc::{Context, Runtime, Function};
    ///
    /// let runtime = Runtime::default();
    /// let ctx = Context::new(&runtime);
    ///
    /// let add = Function::new(&ctx, |ctx, args| {
    ///     let a = args[0].to_number();
    ///     let b = args[1].to_number();
    ///     Ok(rjsc::Value::from_f64(ctx, a + b))
    /// });
    ///
    /// ctx.global().set("add", add).unwrap();
    /// ```
    pub fn new<F>(ctx: &Context, f: F) -> Value<'_>
    where
        F: for<'a> Fn(&'a Context, &[Value<'a>]) -> Result<Value<'a>, String>
            + 'static,
    {
        let cb: Box<RustCallback> = Box::new(f);
        let holder =
            Box::new(CallbackHolder { cb, runtime: ctx._runtime.clone() });
        let private = Box::into_raw(holder) as *mut std::ffi::c_void;

        let mut def = unsafe { kJSClassDefinitionEmpty };
        let class_name = CString::new("RustFunction").unwrap();
        def.className = class_name.as_ptr();
        def.callAsFunction = Some(rust_callback_trampoline);
        def.finalize = Some(rust_callback_finalize);
        let class = unsafe { JSClassCreate(&def) };

        let func_obj = unsafe { JSObjectMake(ctx.raw(), class, private) };
        unsafe { JSClassRelease(class) };
        unsafe { Value::from_raw(ctx, func_obj) }
    }

    /// Creates a raw JS function object from a Rust closure.
    ///
    /// This is a lower-level alternative to [`Function::new`] that works
    /// with raw JSC types. The closure receives the raw context and a
    /// pointer to the first argument (may be null if argc == 0).
    ///
    /// This is primarily useful for building custom abstractions on top
    /// of the raw JSC API.
    pub fn raw<F>(ctx: &Context, f: F) -> JSObjectRef
    where
        F: Fn(JSContextRef, *const JSValueRef) -> JSValueRef + 'static,
    {
        use crate::callbacks::{
            raw_callback_finalize, raw_callback_trampoline,
        };

        let cb: Box<RawCb> = Box::new(f);
        let holder =
            Box::new(CallbackHolder { cb, runtime: ctx._runtime.clone() });
        let private = Box::into_raw(holder) as *mut std::ffi::c_void;

        let mut def = unsafe { kJSClassDefinitionEmpty };
        let name = CString::new("RjscRawCb").unwrap();
        def.className = name.as_ptr();
        def.callAsFunction = Some(raw_callback_trampoline);
        def.finalize = Some(raw_callback_finalize);
        let class = unsafe { JSClassCreate(&def) };
        let obj = unsafe { JSObjectMake(ctx.raw(), class, private) };
        unsafe { JSClassRelease(class) };
        obj
    }
}

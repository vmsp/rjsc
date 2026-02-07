use std::mem;
use std::ptr;

use rjsc_sys::*;

use crate::js_string_from_rust;
use crate::JsContext;
use crate::JsValue;

pub(crate) type RustCallback =
    dyn for<'a> Fn(&'a JsContext, &[JsValue<'a>]) -> Result<JsValue<'a>, String>
        + 'static;

pub(crate) type RawCb =
    dyn Fn(JSContextRef, *const JSValueRef) -> JSValueRef + 'static;

pub(crate) struct CallbackHolder<T: ?Sized> {
    pub(crate) cb: Box<T>,
}

pub(crate) unsafe extern "C" fn rust_callback_trampoline(
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
    let holder =
        unsafe { &*(private as *const CallbackHolder<RustCallback>) };
    let cb = &*holder.cb;

    let temp_ctx = std::mem::ManuallyDrop::new(JsContext {
        raw: ctx as JSGlobalContextRef,
        _not_send_sync: std::marker::PhantomData,
        _runtime: None,
    });

    let args: Vec<JsValue<'_>> = (0..argc)
        .map(|i| {
            let raw = unsafe { *argv.add(i) };
            unsafe { JsValue::from_raw(&temp_ctx, raw) }
        })
        .collect();

    match cb(&temp_ctx, &args) {
        Ok(val) => {
            let raw = val.raw;
            mem::forget(val);
            raw
        }
        Err(msg) => {
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

pub(crate) unsafe extern "C" fn rust_callback_finalize(obj: JSObjectRef) {
    let private = unsafe { JSObjectGetPrivate(obj) };
    if !private.is_null() {
        let _ = unsafe {
            Box::from_raw(private as *mut CallbackHolder<RustCallback>)
        };
    }
}

pub(crate) unsafe extern "C" fn raw_callback_trampoline(
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

pub(crate) unsafe extern "C" fn raw_callback_finalize(obj: JSObjectRef) {
    let private = unsafe { JSObjectGetPrivate(obj) };
    if !private.is_null() {
        let _ = unsafe { Box::from_raw(private as *mut CallbackHolder<RawCb>) };
    }
}

use std::fmt;
use std::ptr;

use rjsc_sys::*;

use crate::js_string_to_rust;

/// A JavaScript execution error.
#[derive(Debug)]
pub struct JsException {
    message: String,
}

impl JsException {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        JsException { message: message.into() }
    }

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

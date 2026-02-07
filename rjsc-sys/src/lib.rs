//! Raw FFI bindings to JavaScriptCore.

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CStr;
    use std::ptr;

    #[test]
    fn evaluate_arithmetic() {
        unsafe {
            let ctx = JSGlobalContextCreate(ptr::null_mut());

            let script = JSStringCreateWithUTF8CString(c"40 + 2".as_ptr());
            let mut exception: JSValueRef = ptr::null();
            let result = JSEvaluateScript(
                ctx,
                script,
                ptr::null_mut(),
                ptr::null_mut(),
                1,
                &mut exception,
            );
            JSStringRelease(script);

            assert!(exception.is_null(), "JS exception was thrown");
            assert!(!result.is_null(), "result should not be null");

            let num = JSValueToNumber(ctx, result, ptr::null_mut());
            assert_eq!(num, 42.0);

            JSGlobalContextRelease(ctx);
        }
    }

    #[test]
    fn evaluate_string_expression() {
        unsafe {
            let ctx = JSGlobalContextCreate(ptr::null_mut());

            let script = JSStringCreateWithUTF8CString(
                c"'hello' + ' ' + 'from' + ' ' + 'JSC'".as_ptr(),
            );
            let mut exception: JSValueRef = ptr::null();
            let result = JSEvaluateScript(
                ctx,
                script,
                ptr::null_mut(),
                ptr::null_mut(),
                1,
                &mut exception,
            );
            JSStringRelease(script);

            assert!(exception.is_null(), "JS exception was thrown");

            let js_str = JSValueToStringCopy(ctx, result, ptr::null_mut());
            let rust_str = js_string_to_string(js_str);
            JSStringRelease(js_str);

            assert_eq!(rust_str, "hello from JSC");

            JSGlobalContextRelease(ctx);
        }
    }

    fn js_string_to_string(js_str: JSStringRef) -> String {
        let max_len = unsafe { JSStringGetMaximumUTF8CStringSize(js_str) };
        let mut buf = vec![0u8; max_len];
        unsafe {
            JSStringGetUTF8CString(js_str, buf.as_mut_ptr() as *mut _, max_len)
        };
        let c_str = CStr::from_bytes_until_nul(&buf).unwrap();
        c_str.to_string_lossy().into_owned()
    }
}

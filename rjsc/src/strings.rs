use std::ffi::CStr;
use std::ffi::CString;

use rjsc_sys::*;

pub(crate) unsafe fn js_string_from_rust(s: &str) -> JSStringRef {
    let c_string =
        CString::new(s).expect("JS string contained interior NUL");
    unsafe { JSStringCreateWithUTF8CString(c_string.as_ptr()) }
}

pub(crate) unsafe fn js_string_to_rust(js_str: JSStringRef) -> String {
    let max_len = unsafe { JSStringGetMaximumUTF8CStringSize(js_str) };
    let mut buf = vec![0u8; max_len];
    unsafe {
        JSStringGetUTF8CString(js_str, buf.as_mut_ptr() as *mut _, max_len);
    }
    let c_str = CStr::from_bytes_until_nul(&buf).unwrap();
    c_str.to_string_lossy().into_owned()
}

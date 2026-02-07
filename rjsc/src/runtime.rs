use std::rc::Rc;

use rjsc_sys::*;

/// A JavaScript runtime (context group).
///
/// Contexts created from the same runtime share a VM and must remain on the
/// same thread.
pub struct JsRuntime {
    inner: Rc<JsRuntimeInner>,
}

pub(crate) struct JsRuntimeInner {
    raw: JSContextGroupRef,
}

impl JsRuntime {
    /// Creates a new JavaScript runtime (context group).
    pub fn new() -> Self {
        let raw = unsafe { JSContextGroupCreate() };
        JsRuntime { inner: Rc::new(JsRuntimeInner { raw }) }
    }

    /// Creates a new context inside this runtime.
    pub fn new_context(&self) -> crate::JsContext {
        crate::JsContext::new_in(self)
    }

    pub(crate) fn inner_clone(&self) -> Rc<JsRuntimeInner> {
        Rc::clone(&self.inner)
    }
}

impl Default for JsRuntime {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for JsRuntimeInner {
    fn drop(&mut self) {
        unsafe { JSContextGroupRelease(self.raw) };
    }
}

impl JsRuntime {
    pub(crate) fn raw(&self) -> JSContextGroupRef {
        self.inner.raw
    }
}

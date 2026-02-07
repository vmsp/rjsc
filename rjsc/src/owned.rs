use std::mem;
use std::ops::Deref;
use std::rc::Rc;

use rjsc_sys::*;

use crate::{JsContext, JsObject, JsValue};

pub struct JsContextOwned {
    ctx: JsContext,
}

impl JsContextOwned {
    pub fn from_ctx(ctx: &JsContext) -> Self {
        let raw = unsafe { JSGlobalContextRetain(ctx.raw()) };
        let runtime = ctx._runtime.clone();
        let ctx = JsContext {
            raw,
            _not_send_sync: std::marker::PhantomData,
            _runtime: runtime,
        };
        JsContextOwned { ctx }
    }
}

impl Deref for JsContextOwned {
    type Target = JsContext;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

pub struct JsValueOwned {
    ctx: JSGlobalContextRef,
    runtime: Option<Rc<crate::runtime::JsRuntimeInner>>,
    raw: JSValueRef,
}

impl JsValueOwned {
    pub fn from_ref(ctx: &JsContext, value: &JsValue<'_>) -> Self {
        unsafe { JSValueProtect(ctx.as_ctx(), value.raw) };
        JsValueOwned {
            ctx: ctx.raw(),
            runtime: ctx._runtime.clone(),
            raw: value.raw,
        }
    }

    pub fn from_value(ctx: &JsContext, value: JsValue<'_>) -> Self {
        let raw = value.raw;
        mem::forget(value);
        JsValueOwned { ctx: ctx.raw(), runtime: ctx._runtime.clone(), raw }
    }

    pub fn into_value(self) -> JsValue<'static> {
        let ctx = mem::ManuallyDrop::new(JsContext {
            raw: self.ctx,
            _not_send_sync: std::marker::PhantomData,
            _runtime: self.runtime.clone(),
        });
        let value = unsafe { JsValue::from_raw(&ctx, self.raw) };
        unsafe { JSValueUnprotect(self.ctx, self.raw) };
        mem::forget(self);
        unsafe { mem::transmute::<JsValue<'_>, JsValue<'static>>(value) }
    }
}

impl Drop for JsValueOwned {
    fn drop(&mut self) {
        unsafe { JSValueUnprotect(self.ctx, self.raw) };
    }
}

pub struct JsObjectOwned {
    ctx: JSGlobalContextRef,
    runtime: Option<Rc<crate::runtime::JsRuntimeInner>>,
    raw: JSObjectRef,
}

impl JsObjectOwned {
    pub fn from_object(ctx: &JsContext, value: JsObject<'_>) -> Self {
        let raw = value.raw;
        mem::forget(value);
        JsObjectOwned { ctx: ctx.raw(), runtime: ctx._runtime.clone(), raw }
    }

    pub fn into_object(self) -> JsObject<'static> {
        let ctx = mem::ManuallyDrop::new(JsContext {
            raw: self.ctx,
            _not_send_sync: std::marker::PhantomData,
            _runtime: self.runtime.clone(),
        });
        let obj = unsafe { JsObject::from_raw(&ctx, self.raw) };
        unsafe { JSValueUnprotect(self.ctx, self.raw) };
        mem::forget(self);
        unsafe { mem::transmute::<JsObject<'_>, JsObject<'static>>(obj) }
    }

    pub fn into_value(self) -> JsValueOwned {
        let value = JsValueOwned {
            ctx: self.ctx,
            runtime: self.runtime.clone(),
            raw: self.raw as JSValueRef,
        };
        mem::forget(self);
        value
    }
}

impl Drop for JsObjectOwned {
    fn drop(&mut self) {
        unsafe { JSValueUnprotect(self.ctx, self.raw) };
    }
}

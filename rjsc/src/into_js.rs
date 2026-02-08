use crate::{Context, Function, Value};

/// Trait for converting Rust types into JavaScript values.
pub trait IntoJs<'ctx> {
    /// Converts this value into a JavaScript [`Value`].
    fn into_js(self, ctx: &'ctx Context) -> Value<'ctx>;
}

impl<'ctx, F> IntoJs<'ctx> for F
where
    F: for<'a> Fn(&'a Context, &[Value<'a>]) -> Result<Value<'a>, String>
        + 'static,
{
    fn into_js(self, ctx: &'ctx Context) -> Value<'ctx> {
        Function::new(ctx, self)
    }
}

impl<'ctx> IntoJs<'ctx> for Value<'ctx> {
    fn into_js(self, _ctx: &'ctx Context) -> Value<'ctx> {
        self
    }
}

impl<'ctx> IntoJs<'ctx> for &Value<'ctx> {
    fn into_js(self, _ctx: &'ctx Context) -> Value<'ctx> {
        // Clone by creating a new protected reference to the same raw value
        unsafe { Value::from_raw(_ctx, self.raw) }
    }
}

impl<'ctx> IntoJs<'ctx> for bool {
    fn into_js(self, ctx: &'ctx Context) -> Value<'ctx> {
        Value::from_bool(ctx, self)
    }
}

impl<'ctx> IntoJs<'ctx> for f64 {
    fn into_js(self, ctx: &'ctx Context) -> Value<'ctx> {
        Value::from_f64(ctx, self)
    }
}

impl<'ctx> IntoJs<'ctx> for i32 {
    fn into_js(self, ctx: &'ctx Context) -> Value<'ctx> {
        Value::from_f64(ctx, self as f64)
    }
}

impl<'ctx> IntoJs<'ctx> for &str {
    fn into_js(self, ctx: &'ctx Context) -> Value<'ctx> {
        Value::from_str(ctx, self)
    }
}

impl<'ctx> IntoJs<'ctx> for String {
    fn into_js(self, ctx: &'ctx Context) -> Value<'ctx> {
        Value::from_str(ctx, &self)
    }
}

use crate::*;
use futures::task::noop_waker;
use std::pin::Pin;
use std::task::{Context as TaskContext, Poll};

fn test_ctx() -> Context {
    let runtime = Runtime::new();
    Context::new_in(&runtime)
}

fn poll_promise_future<'ctx>(
    mut fut: Pin<&mut PromiseFuture<'ctx>>,
    ctx: &'ctx Context,
) -> Result<Value<'ctx>, Exception> {
    let waker = noop_waker();
    let mut cx = TaskContext::from_waker(&waker);
    let driver = MicrotaskDrain::default();

    for _ in 0..driver.max_rounds() {
        match fut.as_mut().poll(&mut cx) {
            Poll::Ready(result) => return result,
            Poll::Pending => {
                driver.drain_jobs(ctx)?;
            }
        }
    }

    Err(Exception::new("Future did not resolve within drain budget."))
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
    assert!(err.message().contains("SyntaxError"), "got: {}", err.message());
}

#[test]
fn eval_runtime_error() {
    let ctx = test_ctx();
    let err = ctx.eval("nonexistent()").unwrap_err();
    assert!(err.message().contains("ReferenceError"), "got: {}", err.message());
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
    let val =
        ctx.eval_async("(async () => { return 'async works'; })()").unwrap();
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
        .eval_async(concat!(
            "Promise.resolve(10).then(x => x * 2).",
            "then(x => x + 1)"
        ))
        .unwrap();
    assert_eq!(val.to_number(), 21.0);
}

#[test]
fn eval_async_non_promise() {
    let ctx = test_ctx();
    let val = ctx.eval_async("42").unwrap();
    assert_eq!(val.to_number(), 42.0);
}

#[test]
fn eval_promise_returns_promise() {
    let ctx = test_ctx();
    let promise = ctx.eval_promise("Promise.resolve(7)").unwrap();
    let val = promise.await_blocking().unwrap();
    assert_eq!(val.to_number(), 7.0);
}

#[test]
fn value_to_promise() {
    let ctx = test_ctx();
    let val = ctx.eval("Promise.resolve(9)").unwrap();
    let promise = val.to_promise().unwrap();
    let val = promise.await_blocking().unwrap();
    assert_eq!(val.to_number(), 9.0);
}

#[test]
fn promise_deferred_resolve() {
    let ctx = test_ctx();
    let (promise, resolver) = Promise::deferred(&ctx).unwrap();
    let mut fut = Box::pin(promise.into_future());
    let waker = noop_waker();
    let mut cx = TaskContext::from_waker(&waker);
    assert!(matches!(fut.as_mut().poll(&mut cx), Poll::Pending));
    let value = Value::from_str(&ctx, "done");
    resolver.resolve(&value).unwrap();
    let val = poll_promise_future(fut.as_mut(), &ctx).unwrap();
    assert_eq!(val.to_string_lossy(), "done");
}

#[test]
fn promise_deferred_reject() {
    let ctx = test_ctx();
    let (promise, resolver) = Promise::deferred(&ctx).unwrap();
    let mut fut = Box::pin(promise.into_future());
    let waker = noop_waker();
    let mut cx = TaskContext::from_waker(&waker);
    assert!(matches!(fut.as_mut().poll(&mut cx), Poll::Pending));
    resolver.reject_str("nope").unwrap();
    let err = poll_promise_future(fut.as_mut(), &ctx).unwrap_err();
    assert_eq!(err.message(), "nope");
}

#[test]
fn promise_into_future() {
    let ctx = test_ctx();
    let promise = ctx.eval_promise("Promise.resolve(123)").unwrap();
    let mut fut = Box::pin(promise.into_future());
    let val = poll_promise_future(fut.as_mut(), &ctx).unwrap();
    assert_eq!(val.to_number(), 123.0);
}

#[test]
fn value_undefined() {
    let ctx = test_ctx();
    let val = Value::undefined(&ctx);
    assert!(val.is_undefined());
    assert!(!val.is_null());
    assert_eq!(val.to_string_lossy(), "undefined");
}

#[test]
fn value_null() {
    let ctx = test_ctx();
    let val = Value::null(&ctx);
    assert!(val.is_null());
    assert!(!val.is_undefined());
    assert_eq!(val.to_string_lossy(), "null");
}

#[test]
fn value_from_bool() {
    let ctx = test_ctx();
    let t = Value::from_bool(&ctx, true);
    assert!(t.is_boolean());
    assert!(t.to_boolean());

    let f = Value::from_bool(&ctx, false);
    assert!(f.is_boolean());
    assert!(!f.to_boolean());
}

#[test]
fn value_from_f64() {
    let ctx = test_ctx();
    let val = Value::from_f64(&ctx, 3.14);
    assert!(val.is_number());
    assert!((val.to_number() - 3.14).abs() < f64::EPSILON);
}

#[test]
fn value_from_str() {
    let ctx = test_ctx();
    let val = Value::from_str(&ctx, "hello from rust");
    assert!(val.is_string());
    assert_eq!(val.to_string_lossy(), "hello from rust");
}

#[test]
fn object_new_and_properties() {
    let ctx = test_ctx();
    let obj = Object::new(&ctx);

    assert!(!obj.has("foo"));

    let val = Value::from_f64(&ctx, 42.0);
    obj.set("foo", &val).unwrap();
    assert!(obj.has("foo"));

    let got = obj.get("foo").unwrap();
    assert_eq!(got.to_number(), 42.0);

    let names = obj.property_names();
    assert_eq!(names, vec!["foo"]);
}

#[test]
fn object_delete_property() {
    let ctx = test_ctx();
    let obj = Object::new(&ctx);
    let val = Value::from_str(&ctx, "bar");
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
    let arr_obj = arr.to_object().unwrap();

    let first = arr_obj.get_index(0).unwrap();
    assert_eq!(first.to_number(), 10.0);

    let third = arr_obj.get_index(2).unwrap();
    assert_eq!(third.to_number(), 30.0);

    let replacement = Value::from_f64(&ctx, 99.0);
    arr_obj.set_index(1, &replacement).unwrap();
    let updated = arr_obj.get_index(1).unwrap();
    assert_eq!(updated.to_number(), 99.0);
}

#[test]
fn object_from_eval() {
    let ctx = test_ctx();
    let val = ctx.eval("({a: 1, b: 'two'})").unwrap();
    let obj = val.to_object().unwrap();

    let a = obj.get("a").unwrap();
    assert_eq!(a.to_number(), 1.0);

    let b = obj.get("b").unwrap();
    assert_eq!(b.to_string_lossy(), "two");
}

#[test]
fn object_call_function() {
    let ctx = test_ctx();
    let func_val = ctx.eval("(function(x) { return x * 2; })").unwrap();
    let func = func_val.to_object().unwrap();

    assert!(func.is_function());

    let arg = Value::from_f64(&ctx, 21.0);
    let result = func.call(None, &[&arg]).unwrap();
    assert_eq!(result.to_number(), 42.0);
}

#[test]
fn object_call_method() {
    let ctx = test_ctx();
    let val = ctx
        .eval(concat!("({value: 10, double() ", "{ return this.value * 2; }})"))
        .unwrap();
    let obj = val.to_object().unwrap();

    let result = obj.call_method("double", &[]).unwrap();
    assert_eq!(result.to_number(), 20.0);
}

#[test]
fn global_set_function_basic() {
    let ctx = test_ctx();
    fn make_add(
    ) -> impl for<'a> Fn(&'a Context, &[Value<'a>]) -> Result<Value<'a>, String>
    {
        |ctx, args| {
            let a = args[0].to_number();
            let b = args[1].to_number();
            Ok(Value::from_f64(ctx, a + b))
        }
    }
    ctx.global().set("add", make_add()).unwrap();
    let result = ctx.eval("add(3, 4)").unwrap();
    assert_eq!(result.to_number(), 7.0);
}

#[test]
fn global_set_function_returns_string() {
    let ctx = test_ctx();
    fn make_greet(
    ) -> impl for<'a> Fn(&'a Context, &[Value<'a>]) -> Result<Value<'a>, String>
    {
        |ctx, args| {
            let name = args[0].to_string_lossy();
            Ok(Value::from_str(ctx, &format!("hi, {name}")))
        }
    }
    ctx.global().set("greet", make_greet()).unwrap();
    let result = ctx.eval("greet('world')").unwrap();
    assert_eq!(result.to_string_lossy(), "hi, world");
}

#[test]
fn global_set_function_error() {
    let ctx = test_ctx();
    fn make_fail(
    ) -> impl for<'a> Fn(&'a Context, &[Value<'a>]) -> Result<Value<'a>, String>
    {
        |_ctx, _args| Err("something went wrong".into())
    }
    ctx.global().set("fail", make_fail()).unwrap();
    let err = ctx.eval("fail()").unwrap_err();
    assert!(
        err.message().contains("something went wrong"),
        "got: {}",
        err.message()
    );
}

#[test]
fn global_set_function_no_args() {
    let ctx = test_ctx();
    fn make_forty_two(
    ) -> impl for<'a> Fn(&'a Context, &[Value<'a>]) -> Result<Value<'a>, String>
    {
        |ctx, _args| Ok(Value::from_f64(ctx, 42.0))
    }
    ctx.global().set("forty_two", make_forty_two()).unwrap();
    let result = ctx.eval("forty_two()").unwrap();
    assert_eq!(result.to_number(), 42.0);
}

#[test]
fn global_set_function_called_from_js_function() {
    let ctx = test_ctx();
    fn make_double(
    ) -> impl for<'a> Fn(&'a Context, &[Value<'a>]) -> Result<Value<'a>, String>
    {
        |ctx, args| {
            let n = args[0].to_number();
            Ok(Value::from_f64(ctx, n * 2.0))
        }
    }
    ctx.global().set("double", make_double()).unwrap();
    let result = ctx.eval("[1,2,3].map(double)").unwrap();
    let arr = result.to_object().unwrap();
    assert_eq!(arr.get_index(0).unwrap().to_number(), 2.0);
    assert_eq!(arr.get_index(1).unwrap().to_number(), 4.0);
    assert_eq!(arr.get_index(2).unwrap().to_number(), 6.0);
}

#[test]
fn global_set_primitives() {
    let ctx = test_ctx();
    let global = ctx.global();

    global.set("myBool", true).unwrap();
    global.set("myInt", 42i32).unwrap();
    global.set("myFloat", 3.14f64).unwrap();
    global.set("myString", "hello").unwrap();
    global.set("myOwnedString", "world".to_string()).unwrap();

    assert!(ctx.eval("myBool === true").unwrap().to_boolean());
    assert_eq!(ctx.eval("myInt").unwrap().to_number(), 42.0);
    assert!(ctx.eval("myFloat === 3.14").unwrap().to_boolean());
    assert!(ctx.eval("myString === 'hello'").unwrap().to_boolean());
    assert!(ctx.eval("myOwnedString === 'world'").unwrap().to_boolean());
}

#[test]
fn global_set_function() {
    let ctx = test_ctx();
    let global = ctx.global();

    fn make_add(
    ) -> impl for<'a> Fn(&'a Context, &[Value<'a>]) -> Result<Value<'a>, String>
    {
        |ctx, args| {
            let a = args[0].to_number();
            let b = args[1].to_number();
            Ok(Value::from_f64(ctx, a + b))
        }
    }
    global.set("add", make_add()).unwrap();

    let result = ctx.eval("add(5, 7)").unwrap();
    assert_eq!(result.to_number(), 12.0);
}

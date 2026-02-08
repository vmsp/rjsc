use rjsc::{function, Context, Runtime};

fn test_ctx() -> Context {
    let runtime = Runtime::default();
    Context::new(&runtime)
}

#[test]
fn test_macro_basic() {
    #[function]
    fn add(a: i32, b: i32) -> i32 {
        a + b
    }

    let ctx = test_ctx();
    ctx.global().set("add", add).unwrap();

    let result = ctx.eval("add(3, 4)").unwrap();
    assert_eq!(result.to_number(), 7.0);
}

#[test]
fn test_macro_with_context() {
    #[function]
    fn greet(_ctx: &Context, name: String) -> String {
        format!("Hello, {name}!")
    }

    let ctx = test_ctx();
    ctx.global().set("greet", greet).unwrap();

    let result = ctx.eval("greet('World')").unwrap();
    assert_eq!(result.to_string_lossy(), "Hello, World!");
}

#[test]
fn test_macro_with_result() {
    #[function]
    fn divide(a: f64, b: f64) -> Result<f64, String> {
        if b == 0.0 {
            Err("Division by zero".to_string())
        } else {
            Ok(a / b)
        }
    }

    let ctx = test_ctx();
    ctx.global().set("divide", divide).unwrap();

    let result = ctx.eval("divide(10, 2)").unwrap();
    assert_eq!(result.to_number(), 5.0);

    let err = ctx.eval("divide(10, 0)").unwrap_err();
    assert!(err.message().contains("Division by zero"));
}

#[test]
fn test_macro_bool() {
    #[function]
    fn is_even(n: i32) -> bool {
        n % 2 == 0
    }

    let ctx = test_ctx();
    ctx.global().set("is_even", is_even).unwrap();

    assert!(ctx.eval("is_even(4)").unwrap().to_boolean());
    assert!(!ctx.eval("is_even(5)").unwrap().to_boolean());
}

#[test]
fn test_macro_no_args() {
    #[function]
    fn get_answer() -> i32 {
        42
    }

    let ctx = test_ctx();
    ctx.global().set("get_answer", get_answer).unwrap();

    let result = ctx.eval("get_answer()").unwrap();
    assert_eq!(result.to_number(), 42.0);
}

#[test]
fn test_macro_f64() {
    #[function]
    fn calculate(a: f64, b: f64) -> f64 {
        a * b + 0.5
    }

    let ctx = test_ctx();
    ctx.global().set("calculate", calculate).unwrap();

    let result = ctx.eval("calculate(2.0, 3.0)").unwrap();
    assert!(result.to_number() - 6.5 < 0.001);
}

#[test]
fn test_macro_async_basic() {
    #[function]
    async fn async_add(a: i32, b: i32) -> i32 {
        // Simulate some async work
        a + b
    }

    let ctx = test_ctx();
    ctx.global().set("async_add", async_add).unwrap();

    // Call the async function - it returns a Promise
    let promise = ctx.eval_promise("async_add(3, 4)").unwrap();
    let rt = ctx.runtime().unwrap();
    let result = rt.block_on(&ctx, promise.into_future()).unwrap();
    assert_eq!(result.to_number(), 7.0);
}

#[test]
fn test_macro_async_string() {
    #[function]
    async fn async_greet(name: String) -> String {
        format!("Hello, {name}!")
    }

    let ctx = test_ctx();
    ctx.global().set("async_greet", async_greet).unwrap();

    let promise = ctx.eval_promise("async_greet('World')").unwrap();
    let rt = ctx.runtime().unwrap();
    let result = rt.block_on(&ctx, promise.into_future()).unwrap();
    assert_eq!(result.to_string_lossy(), "Hello, World!");
}

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, FnArg, ItemFn, ReturnType, Type};

#[proc_macro_attribute]
pub fn function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);

    let fn_name = &input_fn.sig.ident;
    let fn_vis = &input_fn.vis;
    let fn_block = &input_fn.block;
    let is_async = input_fn.sig.asyncness.is_some();

    // Create internal name for the actual function
    let internal_name = format_ident!("__rjsc_{}", fn_name);
    let wrapper_name = format_ident!("__rjsc_wrap_{}", fn_name);
    let struct_name = format_ident!("__rjsc_fn_{}", fn_name);

    // Build the original function signature for the internal implementation
    let fn_sig = &input_fn.sig;
    let fn_inputs = &fn_sig.inputs;
    let fn_output = &fn_sig.output;

    // Modify the signature to use internal name
    let mut internal_sig = fn_sig.clone();
    internal_sig.ident = internal_name.clone();

    // Extract argument analysis
    let mut arg_converters = Vec::new();
    let mut arg_count = 0;

    for (idx, arg) in fn_inputs.iter().enumerate() {
        match arg {
            FnArg::Receiver(_) => {
                panic!("#[rjsc::function] cannot be used on methods")
            }
            FnArg::Typed(pat_type) => {
                let pat = &pat_type.pat;
                let ty = &pat_type.ty;

                // Check if first arg is &Context
                if idx == 0 && is_context_ref(ty) {
                    arg_converters.push(ArgConverter {
                        idx,
                        converter: quote! { #pat },
                        is_ctx: true,
                    });
                } else {
                    let converter = generate_arg_converter(
                        idx,
                        arg_count,
                        ty,
                        quote! { #pat },
                    );
                    arg_converters.push(ArgConverter {
                        idx,
                        converter,
                        is_ctx: false,
                    });
                    arg_count += 1;
                }
            }
        }
    }

    // Generate argument extraction code
    let arg_extraction: Vec<_> = arg_converters
        .iter()
        .filter(|c| !c.is_ctx)
        .map(|c| &c.converter)
        .collect();

    // Generate the internal function call arguments
    let call_args: Vec<_> = arg_converters
        .iter()
        .map(|c| {
            if c.is_ctx {
                quote! { ctx }
            } else {
                let idx = c.idx;
                let arg_name = format_ident!("arg{}", idx);
                quote! { #arg_name }
            }
        })
        .collect();

    // Generate the wrapper body based on sync vs async
    let wrapper_body = if is_async {
        generate_async_wrapper_body(
            &internal_name,
            &call_args,
            fn_output,
            &arg_extraction,
        )
    } else {
        generate_sync_wrapper_body(
            &internal_name,
            &call_args,
            fn_output,
            &arg_extraction,
        )
    };

    // Generate the output
    let output = quote! {
        // The internal function with the actual implementation
        #[doc(hidden)]
        #fn_vis #internal_sig {
            #fn_block
        }

        // Wrapper function that bridges JS -> Rust
        #[doc(hidden)]
        #fn_vis fn #wrapper_name<'ctx>(
            ctx: &'ctx ::rjsc::Context,
            args: &[::rjsc::Value<'ctx>],
        ) -> ::std::result::Result<::rjsc::Value<'ctx>, String> {
            #wrapper_body
        }

        // Marker struct for IntoJs implementation
        #[doc(hidden)]
        #fn_vis struct #struct_name;

        // Implement IntoJs for the marker struct
        impl<'ctx> ::rjsc::IntoJs<'ctx> for #struct_name {
            fn into_js(self, ctx: &'ctx ::rjsc::Context) -> ::rjsc::Value<'ctx> {
                ::rjsc::Function::new(ctx, #wrapper_name)
            }
        }

        // Const instance with the original function name
        #fn_vis const #fn_name: #struct_name = #struct_name;
    };

    output.into()
}

fn generate_sync_wrapper_body(
    internal_name: &syn::Ident,
    call_args: &[proc_macro2::TokenStream],
    fn_output: &ReturnType,
    arg_extraction: &[&proc_macro2::TokenStream],
) -> proc_macro2::TokenStream {
    // Determine return type handling
    let return_handling = match fn_output {
        ReturnType::Default => ReturnHandling::Unit,
        ReturnType::Type(_, ty) => analyze_return_type(ty),
    };

    // Generate return conversion
    match &return_handling {
        ReturnHandling::Unit => quote! {
            #(#arg_extraction)*
            #internal_name(#(#call_args),*);
            ::std::result::Result::Ok(::rjsc::Value::undefined(ctx))
        },
        ReturnHandling::Direct(ty) => {
            let conv = generate_return_converter(ty);
            quote! {
                #(#arg_extraction)*
                let result = #internal_name(#(#call_args),*);
                ::std::result::Result::Ok(#conv)
            }
        }
        ReturnHandling::Result(ok_ty) => {
            let conv = generate_return_converter(ok_ty);
            quote! {
                #(#arg_extraction)*
                match #internal_name(#(#call_args),*) {
                    ::std::result::Result::Ok(result) => ::std::result::Result::Ok(#conv),
                    ::std::result::Result::Err(e) => ::std::result::Result::Err(e.to_string()),
                }
            }
        }
    }
}

fn generate_async_wrapper_body(
    internal_name: &syn::Ident,
    call_args: &[proc_macro2::TokenStream],
    fn_output: &ReturnType,
    arg_extraction: &[&proc_macro2::TokenStream],
) -> proc_macro2::TokenStream {
    // Determine the output type of the async function
    let return_handling = match fn_output {
        ReturnType::Default => ReturnHandling::Unit,
        ReturnType::Type(_, ty) => {
            // For async functions, the return type is wrapped in impl Future<Output = ...>
            // but we see the inner type here
            analyze_return_type(ty)
        }
    };

    // Generate the code that converts the async result to TaskResult
    let result_conv = match &return_handling {
        ReturnHandling::Unit => quote! {
            fut.await;
            ::rjsc::task::TaskResult::Ok(::rjsc::task::TaskValue::Undefined)
        },
        ReturnHandling::Direct(ty) => {
            let conv = generate_task_result_converter(ty);
            quote! {
                let result = fut.await;
                #conv
            }
        }
        ReturnHandling::Result(ok_ty) => {
            let conv = generate_task_result_converter(ok_ty);
            quote! {
                match fut.await {
                    ::std::result::Result::Ok(result) => #conv,
                    ::std::result::Result::Err(e) => ::rjsc::task::TaskResult::Err(e.to_string()),
                }
            }
        }
    };

    quote! {
        #(#arg_extraction)*

        // Get the runtime from context
        let runtime = ctx.runtime()
            .ok_or_else(|| "Context has no runtime".to_string())?;

        // Call the async function to get the future
        let fut = #internal_name(#(#call_args),*);

        // Spawn the async work and return the promise
        let promise_val = runtime.spawn_async(ctx, async move {
            #result_conv
        }).map_err(|e| e.to_string())?;

        ::std::result::Result::Ok(promise_val)
    }
}

struct ArgConverter {
    idx: usize,
    converter: proc_macro2::TokenStream,
    is_ctx: bool,
}

#[derive(Debug)]
enum ReturnHandling {
    Unit,
    Direct(Type),
    Result(Type),
}

fn is_context_ref(ty: &Type) -> bool {
    if let Type::Reference(type_ref) = ty {
        if type_ref.mutability.is_none() {
            if let Type::Path(type_path) = &*type_ref.elem {
                let path = &type_path.path;
                if path.is_ident("Context") {
                    return true;
                }
                // Check for Context with any prefix
                if let Some(last) = path.segments.last() {
                    if last.ident == "Context" {
                        return true;
                    }
                }
            }
        }
    }
    false
}

fn analyze_return_type(ty: &Type) -> ReturnHandling {
    if let Type::Path(type_path) = ty {
        if let Some(seg) = type_path.path.segments.last() {
            if seg.ident == "Result" {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments
                {
                    if let Some(first_arg) = args.args.first() {
                        if let syn::GenericArgument::Type(t) = first_arg {
                            return ReturnHandling::Result(t.clone());
                        }
                    }
                }
            }
        }
    }
    ReturnHandling::Direct(ty.clone())
}

fn generate_arg_converter(
    _orig_idx: usize,
    arg_idx: usize,
    ty: &Type,
    _pat: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let arg_name = format_ident!("arg{}", _orig_idx);
    let idx_lit = syn::Index::from(arg_idx);
    let convert_code = generate_type_conversion(ty, quote! { args[#idx_lit] });

    quote! {
        let #arg_name = #convert_code;
    }
}

fn generate_type_conversion(
    ty: &Type,
    value_expr: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    if let Type::Path(type_path) = ty {
        if let Some(seg) = type_path.path.segments.last() {
            let type_name = seg.ident.to_string();
            return match type_name.as_str() {
                "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16"
                | "u32" | "u64" | "usize" => {
                    quote! { #value_expr.to_number() as #ty }
                }
                "f32" | "f64" => {
                    quote! { #value_expr.to_number() }
                }
                "bool" => {
                    quote! { #value_expr.to_boolean() }
                }
                "String" => {
                    quote! { #value_expr.to_string_lossy() }
                }
                _ => panic!("Unsupported argument type: {}", type_name),
            };
        }
    }
    panic!("Unsupported argument type")
}

fn generate_return_converter(ty: &Type) -> proc_macro2::TokenStream {
    if let Type::Path(type_path) = ty {
        if let Some(seg) = type_path.path.segments.last() {
            let type_name = seg.ident.to_string();
            return match type_name.as_str() {
                "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16"
                | "u32" | "u64" | "usize" | "f32" | "f64" => {
                    quote! { ::rjsc::Value::from_f64(ctx, result as f64) }
                }
                "bool" => {
                    quote! { ::rjsc::Value::from_bool(ctx, result) }
                }
                "String" => {
                    quote! { ::rjsc::Value::from_str(ctx, &result) }
                }
                "str" => {
                    quote! { ::rjsc::Value::from_str(ctx, result) }
                }
                _ => panic!("Unsupported return type: {}", type_name),
            };
        }
    }
    panic!("Unsupported return type")
}

fn generate_task_result_converter(ty: &Type) -> proc_macro2::TokenStream {
    if let Type::Path(type_path) = ty {
        if let Some(seg) = type_path.path.segments.last() {
            let type_name = seg.ident.to_string();
            return match type_name.as_str() {
                "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16"
                | "u32" | "u64" | "usize" | "f32" | "f64" => {
                    quote! { ::rjsc::task::TaskResult::Ok(::rjsc::task::TaskValue::F64(result as f64)) }
                }
                "bool" => {
                    quote! { ::rjsc::task::TaskResult::Ok(::rjsc::task::TaskValue::Bool(result)) }
                }
                "String" => {
                    quote! { ::rjsc::task::TaskResult::Ok(::rjsc::task::TaskValue::String(result)) }
                }
                _ => panic!("Unsupported async return type: {}", type_name),
            };
        }
    }
    panic!("Unsupported async return type")
}

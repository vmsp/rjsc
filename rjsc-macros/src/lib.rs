use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, FnArg, ItemFn, ReturnType, Type};

#[proc_macro_attribute]
pub fn function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);

    let fn_name = &input_fn.sig.ident;
    let fn_vis = &input_fn.vis;
    let fn_block = &input_fn.block;

    // Create internal name for the actual function
    let internal_name = format_ident!("__rjsc_{}", fn_name);
    let wrapper_name = format_ident!("__rjsc_wrap_{}", fn_name);
    let struct_name = format_ident!("__rjsc_fn_{}", fn_name);

    // Build the original function signature for the internal implementation
    let fn_sig = &input_fn.sig;
    let fn_generics = &fn_sig.generics;
    let fn_inputs = &fn_sig.inputs;
    let fn_output = &fn_sig.output;

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

    // Determine return type handling
    let return_handling = match fn_output {
        ReturnType::Default => ReturnHandling::Unit,
        ReturnType::Type(_, ty) => analyze_return_type(ty),
    };

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

    // Generate return conversion
    let return_conv = match &return_handling {
        ReturnHandling::Unit => quote! {
            #internal_name(#(#call_args),*);
            ::std::result::Result::Ok(::rjsc::Value::undefined(ctx))
        },
        ReturnHandling::Direct(ty) => {
            let conv = generate_return_converter(ty);
            quote! {
                let result = #internal_name(#(#call_args),*);
                ::std::result::Result::Ok(#conv)
            }
        }
        ReturnHandling::Result(ok_ty) => {
            let conv = generate_return_converter(ok_ty);
            quote! {
                match #internal_name(#(#call_args),*) {
                    ::std::result::Result::Ok(result) => ::std::result::Result::Ok(#conv),
                    ::std::result::Result::Err(e) => ::std::result::Result::Err(e.to_string()),
                }
            }
        }
    };

    // Generate the output
    let output = quote! {
        // The original function, renamed
        #[doc(hidden)]
        #fn_vis fn #internal_name #fn_generics (#fn_inputs) #fn_output {
            #fn_block
        }

        // Wrapper function
        #[doc(hidden)]
        #fn_vis fn #wrapper_name<'ctx>(
            ctx: &'ctx ::rjsc::Context,
            args: &[::rjsc::Value<'ctx>],
        ) -> ::std::result::Result<::rjsc::Value<'ctx>, String> {
            #(#arg_extraction)*
            #return_conv
        }

        // Marker struct for IntoJs implementation
        #[doc(hidden)]
        #fn_vis struct #struct_name;

        // Implement IntoJs for the marker struct
        impl<'ctx> ::rjsc::IntoJs<'ctx> for #struct_name {
            fn into_js(self, ctx: &'ctx ::rjsc::Context) -> ::rjsc::Value<'ctx> {
                ctx.create_function(#wrapper_name)
            }
        }

        // Const instance with the original function name
        #fn_vis const #fn_name: #struct_name = #struct_name;
    };

    output.into()
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

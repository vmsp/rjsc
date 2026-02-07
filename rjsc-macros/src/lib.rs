use proc_macro::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::quote;
use syn::parse_macro_input;
use syn::{
    FnArg, Ident, ItemFn, Pat, PathArguments, ReturnType, Type, TypePath,
    TypeReference,
};

#[proc_macro_attribute]
pub fn function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    expand_function(input).into()
}

fn expand_function(func: ItemFn) -> proc_macro2::TokenStream {
    let crate_path = match crate_name("rjsc") {
        Ok(FoundCrate::Itself) => quote!(crate),
        Ok(FoundCrate::Name(name)) => {
            let ident = Ident::new(&name, proc_macro2::Span::call_site());
            quote!(::#ident)
        }
        Err(_) => quote!(::rjsc),
    };

    let sig = &func.sig;
    let fn_name = &sig.ident;
    let wrapper_name = Ident::new(&format!("{}_rjsc", fn_name), fn_name.span());
    let register_name =
        Ident::new(&format!("register_{}", fn_name), fn_name.span());

    let mut arg_idents = Vec::new();
    let mut arg_types = Vec::new();
    for arg in sig.inputs.iter() {
        if let FnArg::Typed(typed) = arg {
            if let Pat::Ident(ident) = typed.pat.as_ref() {
                arg_idents.push(ident.ident.clone());
                arg_types.push(typed.ty.as_ref().clone());
            } else {
                return quote! {
                    compile_error!("rjsc::function requires named args");
                    #func
                };
            }
        }
    }

    let mut arg_offset = 0usize;
    let mut prep = Vec::new();

    if let Some(first_ty) = arg_types.first() {
        if is_ctx_ref(first_ty) {
            arg_offset = 1;
        }
    }

    if sig.asyncness.is_some() {
        prep.push(quote! {
            let ctx_owned = #crate_path::JsContextOwned::from_ctx(ctx);
        });
    }

    for (idx, (ident, ty)) in
        arg_idents.iter().zip(arg_types.iter()).enumerate()
    {
        if idx == 0 && arg_offset == 1 {
            continue;
        }
        let arg_index = idx - arg_offset;
        let arg_get = quote! {
            let #ident = args.get(#arg_index).ok_or_else(|| {
                format!("missing argument {}", #arg_index)
            })?;
        };

        let convert = if is_string(ty) {
            quote! { let #ident = #ident.to_string_lossy(); }
        } else if is_bool(ty) {
            quote! { let #ident = #ident.to_boolean(); }
        } else if is_f64(ty) {
            quote! { let #ident = #ident.to_number(); }
        } else if is_js_object(ty) {
            if sig.asyncness.is_some() {
                quote! {
                    let #ident = #crate_path::JsValueOwned::from_ref(
                        ctx,
                        #ident,
                    );
                    let #ident = #ident.into_value();
                    let #ident = #ident
                        .to_object(ctx)
                        .map_err(|e| e.message().to_string())?;
                    let #ident = #crate_path::JsObjectOwned::from_object(
                        ctx,
                        #ident,
                    );
                }
            } else {
                quote! {
                    let #ident = #crate_path::JsValueOwned::from_ref(
                        ctx,
                        #ident,
                    );
                    let #ident = #ident.into_value();
                    let #ident = #ident
                        .to_object(ctx)
                        .map_err(|e| e.message().to_string())?;
                }
            }
        } else if is_jsvalue(ty) {
            if sig.asyncness.is_some() {
                quote! {
                    let #ident = #crate_path::JsValueOwned::from_ref(
                        ctx,
                        #ident,
                    );
                }
            } else {
                return quote! {
                    compile_error!(
                        "rjsc::function does not support JsValue args"
                    );
                    #func
                };
            }
        } else {
            return quote! {
                compile_error!(
                    "rjsc::function only supports String, bool, f64, \
                     JsObject, or &JsContext"
                );
                #func
            };
        };

        prep.push(arg_get);
        prep.push(convert);
    }

    let mut call_args = Vec::new();
    for (idx, (ident, ty)) in
        arg_idents.iter().zip(arg_types.iter()).enumerate()
    {
        if idx == 0 && arg_offset == 1 {
            if sig.asyncness.is_some() {
                call_args.push(quote!(ctx_ref));
            } else {
                call_args.push(quote!(#ident));
            }
            continue;
        }
        if sig.asyncness.is_some() {
            if is_jsvalue(ty) {
                call_args.push(quote!(#ident.into_value()));
            } else if is_js_object(ty) {
                call_args.push(quote!(#ident.into_object()));
            } else {
                call_args.push(quote!(#ident));
            }
        } else {
            call_args.push(quote!(#ident));
        }
    }

    let call_expr = quote! { #fn_name(#(#call_args),*) };
    let ret_expr = build_return_expr(
        &sig.output,
        call_expr,
        sig.asyncness.is_some(),
        &crate_path,
    );

    let func_vis = &func.vis;

    quote! {
        #func

        #func_vis fn #wrapper_name<'ctx>(
            ctx: &'ctx #crate_path::JsContext,
            args: &[#crate_path::JsValue<'ctx>],
        ) -> Result<#crate_path::JsValue<'ctx>, String> {
            #(#prep)*
            #ret_expr
        }

        #func_vis fn #register_name(ctx: &#crate_path::JsContext) {
            ctx.register_fn(stringify!(#fn_name), #wrapper_name);
        }
    }
}

fn build_return_expr(
    output: &ReturnType,
    call_expr: proc_macro2::TokenStream,
    is_async: bool,
    crate_path: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let convert = if is_async {
        match output {
            ReturnType::Default => quote! {
                let __rjsc_convert_result =
                    |_result: ()| -> #crate_path::TaskResult {
                        #crate_path::TaskResult::Ok(
                            #crate_path::TaskValue::Undefined
                        )
                    };
            },
            ReturnType::Type(_, ty) => {
                if is_result_jsvalue(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: Result<#crate_path::JsValue<'_>, String>|
                                -> #crate_path::TaskResult {
                                    match result {
                                        Ok(val) => {
                                            #crate_path::TaskResult::Value(
                                                #crate_path::JsValueOwned
                                                    ::from_value(
                                                    ctx_ref,
                                                    val,
                                                )
                                            )
                                        }
                                        Err(err) => {
                                            #crate_path::TaskResult::Err(err)
                                        }
                                    }
                                };
                    }
                } else if is_jsvalue(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: #crate_path::JsValue<'_>|
                                -> #crate_path::TaskResult {
                                    #crate_path::TaskResult::Value(
                                        #crate_path::JsValueOwned::from_value(
                                            ctx_ref,
                                            result,
                                        )
                                    )
                                };
                    }
                } else if is_result_jsobject(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: Result<#crate_path::JsObject<'_>, String>|
                                -> #crate_path::TaskResult {
                                    match result {
                                        Ok(val) => {
                                            #crate_path::TaskResult::Value(
                                                #crate_path::JsObjectOwned
                                                    ::from_object(
                                                    ctx_ref,
                                                    val,
                                                )
                                                .into_value()
                                            )
                                        }
                                        Err(err) => {
                                            #crate_path::TaskResult::Err(err)
                                        }
                                    }
                                };
                    }
                } else if is_js_object(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: #crate_path::JsObject<'_>|
                                -> #crate_path::TaskResult {
                                    #crate_path::TaskResult::Value(
                                        #crate_path::JsObjectOwned::from_object(
                                            ctx_ref,
                                            result,
                                        )
                                        .into_value()
                                    )
                                };
                    }
                } else if is_result_unit(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: Result<(), String>|
                                -> #crate_path::TaskResult {
                                    match result {
                                        Ok(()) => #crate_path::TaskResult::Ok(
                                            #crate_path::TaskValue::Undefined
                                        ),
                                        Err(err) => {
                                            #crate_path::TaskResult::Err(err)
                                        }
                                    }
                                };
                    }
                } else if is_result_string(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: Result<String, String>|
                                -> #crate_path::TaskResult {
                                    match result {
                                        Ok(val) => #crate_path::TaskResult::Ok(
                                            #crate_path::TaskValue::String(val)
                                        ),
                                        Err(err) => {
                                            #crate_path::TaskResult::Err(err)
                                        }
                                    }
                                };
                    }
                } else if is_result_bool(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: Result<bool, String>|
                                -> #crate_path::TaskResult {
                                    match result {
                                        Ok(val) => #crate_path::TaskResult::Ok(
                                            #crate_path::TaskValue::Bool(val)
                                        ),
                                        Err(err) => {
                                            #crate_path::TaskResult::Err(err)
                                        }
                                    }
                                };
                    }
                } else if is_result_f64(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: Result<f64, String>|
                                -> #crate_path::TaskResult {
                                    match result {
                                        Ok(val) => #crate_path::TaskResult::Ok(
                                            #crate_path::TaskValue::F64(val)
                                        ),
                                        Err(err) => {
                                            #crate_path::TaskResult::Err(err)
                                        }
                                    }
                                };
                    }
                } else if is_string(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: String|
                                -> #crate_path::TaskResult {
                                    #crate_path::TaskResult::Ok(
                                        #crate_path::TaskValue::String(result)
                                    )
                                };
                    }
                } else if is_bool(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: bool|
                                -> #crate_path::TaskResult {
                                    #crate_path::TaskResult::Ok(
                                        #crate_path::TaskValue::Bool(result)
                                    )
                                };
                    }
                } else if is_f64(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: f64|
                                -> #crate_path::TaskResult {
                                    #crate_path::TaskResult::Ok(
                                        #crate_path::TaskValue::F64(result)
                                    )
                                };
                    }
                } else if is_unit(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |_result: ()| -> #crate_path::TaskResult {
                                #crate_path::TaskResult::Ok(
                                    #crate_path::TaskValue::Undefined
                                )
                            };
                    }
                } else {
                    return quote! {
                        compile_error!(
                            "async rjsc::function return type must be \
                             (), JsValue, JsObject, String, bool, f64, \
                             or Result<_, String>"
                        );
                    };
                }
            }
        }
    } else {
        match output {
            ReturnType::Default => quote! {
                let __rjsc_convert_result =
                    |_result: ()|
                        -> Result<#crate_path::JsValue<'ctx>, String> {
                        Ok(#crate_path::JsValue::undefined(ctx))
                    };
            },
            ReturnType::Type(_, ty) => {
                if is_result_jsvalue(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: Result<#crate_path::JsValue<'ctx>, String>|
                                -> Result<#crate_path::JsValue<'ctx>, String> {
                                    result
                                };
                    }
                } else if is_result_unit(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: Result<(), String>|
                                -> Result<#crate_path::JsValue<'ctx>, String> {
                                    result.map(|_| {
                                        #crate_path::JsValue::undefined(ctx)
                                    })
                                };
                    }
                } else if is_jsvalue(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |result: #crate_path::JsValue<'ctx>|
                                -> Result<#crate_path::JsValue<'ctx>, String> {
                                    Ok(result)
                                };
                    }
                } else if is_unit(ty) {
                    quote! {
                        let __rjsc_convert_result =
                            |_result: ()|
                                -> Result<#crate_path::JsValue<'ctx>, String> {
                                    Ok(#crate_path::JsValue::undefined(ctx))
                                };
                    }
                } else {
                    return quote! {
                        compile_error!(
                            "rjsc::function return type must be JsValue, \
                             Result<JsValue, String>, Result<(), String>, \
                             or ()"
                        );
                    };
                }
            }
        }
    };

    if is_async {
        quote! {
            let (promise, resolver) = #crate_path::JsPromise::deferred(ctx)
                .map_err(|e| e.message().to_string())?;
            let resolver = resolver.to_owned(ctx);

            let fut: std::pin::Pin<
                Box<dyn std::future::Future<
                    Output = #crate_path::TaskResult
                > + '_>
            > = Box::pin(async move {
                let ctx_ref = &*ctx_owned;
                #convert
                __rjsc_convert_result(#call_expr.await)
            });

            let fut: std::pin::Pin<
                Box<dyn std::future::Future<
                    Output = #crate_path::TaskResult
                > + 'static>
            > = unsafe { std::mem::transmute(fut) };
            let task = #crate_path::Task::new(fut, resolver);
            ctx.__rjsc_enqueue_task(task);

            Ok(promise.to_value(ctx))
        }
    } else {
        quote! {
            #convert
            __rjsc_convert_result(#call_expr)
        }
    }
}

fn is_ctx_ref(ty: &Type) -> bool {
    matches!(ty, Type::Reference(TypeReference { elem, .. })
        if is_jscontext(elem))
}

fn is_jscontext(ty: &Type) -> bool {
    matches!(ty, Type::Path(TypePath { path, .. })
        if path.segments.last().is_some_and(|s| s.ident == "JsContext"))
}

fn is_js_object(ty: &Type) -> bool {
    matches!(ty, Type::Path(TypePath { path, .. })
        if path.segments.last().is_some_and(|s| s.ident == "JsObject"))
}

fn is_jsvalue(ty: &Type) -> bool {
    matches!(ty, Type::Path(TypePath { path, .. })
        if path.segments.last().is_some_and(|s| s.ident == "JsValue"))
}

fn is_string(ty: &Type) -> bool {
    matches!(ty, Type::Path(TypePath { path, .. })
        if path.segments.last().is_some_and(|s| s.ident == "String"))
}

fn is_bool(ty: &Type) -> bool {
    matches!(ty, Type::Path(TypePath { path, .. })
        if path.segments.last().is_some_and(|s| s.ident == "bool"))
}

fn is_f64(ty: &Type) -> bool {
    matches!(ty, Type::Path(TypePath { path, .. })
        if path.segments.last().is_some_and(|s| s.ident == "f64"))
}

fn is_unit(ty: &Type) -> bool {
    matches!(ty, Type::Tuple(t) if t.elems.is_empty())
}

fn is_result_jsvalue(ty: &Type) -> bool {
    is_result_with(ty, "JsValue")
}

fn is_result_jsobject(ty: &Type) -> bool {
    is_result_with(ty, "JsObject")
}

fn is_result_unit(ty: &Type) -> bool {
    matches!(ty, Type::Path(TypePath { path, .. }) if {
        let seg = match path.segments.last() {
            Some(seg) => seg,
            None => return false,
        };
        if seg.ident != "Result" {
            return false;
        }
        let PathArguments::AngleBracketed(args) = &seg.arguments else {
            return false;
        };
        let mut iter = args.args.iter();
        let Some(syn::GenericArgument::Type(first)) = iter.next() else {
            return false;
        };
        let Some(syn::GenericArgument::Type(second)) = iter.next() else {
            return false;
        };
        is_unit(first) && matches!(second, Type::Path(TypePath { path, .. })
            if path.segments.last().is_some_and(|s| s.ident == "String"))
    })
}

fn is_result_string(ty: &Type) -> bool {
    is_result_with(ty, "String")
}

fn is_result_bool(ty: &Type) -> bool {
    is_result_with(ty, "bool")
}

fn is_result_f64(ty: &Type) -> bool {
    is_result_with(ty, "f64")
}

fn is_result_with(ty: &Type, name: &str) -> bool {
    matches!(ty, Type::Path(TypePath { path, .. }) if {
        let seg = match path.segments.last() {
            Some(seg) => seg,
            None => return false,
        };
        if seg.ident != "Result" {
            return false;
        }
        let PathArguments::AngleBracketed(args) = &seg.arguments else {
            return false;
        };
        let mut iter = args.args.iter();
        let Some(syn::GenericArgument::Type(first)) = iter.next() else {
            return false;
        };
        let Some(syn::GenericArgument::Type(second)) = iter.next() else {
            return false;
        };
        let ok = matches!(first, Type::Path(TypePath { path, .. })
            if path.segments.last().is_some_and(|s| s.ident == name));
        let err = matches!(second, Type::Path(TypePath { path, .. })
            if path.segments.last().is_some_and(|s| s.ident == "String"));
        ok && err
    })
}

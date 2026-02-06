use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let dst = cmake::Config::new("WebKit")
        .generator("Ninja")
        .define("PORT", "JSCOnly")
        .define("ENABLE_STATIC_JSC", "ON")
        .define("ENABLE_API_TESTS", "OFF")
        .profile("Release")
        .no_default_flags(true)
        .build_target("JavaScriptCore")
        .build();

    let lib_dir = dst.join("build/lib");
    println!("cargo:rustc-link-search=native={}", lib_dir.display());

    println!("cargo:rustc-link-lib=static=JavaScriptCore");
    println!("cargo:rustc-link-lib=static=WTF");
    println!("cargo:rustc-link-lib=static=bmalloc");

    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
    match target_os.as_str() {
        "linux" => {
            println!("cargo:rustc-link-lib=dylib=icudata");
            println!("cargo:rustc-link-lib=dylib=icui18n");
            println!("cargo:rustc-link-lib=dylib=icuuc");
            println!("cargo:rustc-link-lib=dylib=dl");
            println!("cargo:rustc-link-lib=dylib=pthread");
            println!("cargo:rustc-link-lib=dylib=stdc++");
        }
        // TODO(vitor): Do I care about macOS? It already include JSC
        "macos" => {
            println!("cargo:rustc-link-lib=dylib=icucore");
            println!("cargo:rustc-link-lib=framework=Foundation");
            println!("cargo:rustc-link-lib=dylib=objc");
            println!("cargo:rustc-link-lib=dylib=c++");
        }
        other => {
            panic!("Unsupported target OS: {other}");
        }
    }

    let jsc_headers = dst.join("build/JavaScriptCore/Headers");
    println!("cargo:include={}", jsc_headers.display());

    let mut builder = bindgen::Builder::default()
        .header(
            jsc_headers
                .join("JavaScriptCore/JavaScript.h")
                .to_str()
                .unwrap(),
        )
        .clang_arg(format!("-I{}", jsc_headers.display()))
        // We're building from source, not as an Apple framework.
        .clang_arg("-DSTATICALLY_LINKED_WITH_JavaScriptCore")
        .allowlist_function("JS.*")
        .allowlist_type("JS.*")
        .allowlist_type("OpaqueJS.*")
        .allowlist_var("kJS.*");

    // On macOS, WebKitAvailability.h includes CoreFoundation headers.
    // Point clang at the SDK so it can find them.
    if target_os == "macos" {
        if let Ok(output) = Command::new("xcrun").args(["--show-sdk-path"]).output() {
            let sdk_path = String::from_utf8_lossy(&output.stdout).trim().to_string();
            builder = builder.clang_arg(format!("-isysroot{sdk_path}"));
        }
    }

    let bindings = builder.generate().expect("Failed to generate bindings");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_dir.join("bindings.rs"))
        .expect("Failed to write bindings");

    println!("cargo:rerun-if-changed=build.rs");
}

# A shell for building the page: GHC's wasm toolchain, and libopus for
# wasm32-wasi.
{ pkgs ? import <nixpkgs> { } }:
let
  ghcWasm = (builtins.getFlake "gitlab:haskell-wasm/ghc-wasm-meta/8079eb96bbc63823d99ad8acaff0765532625474?host=gitlab.haskell.org").packages.${pkgs.stdenv.hostPlatform.system};

  opus = pkgs.stdenvNoCC.mkDerivation {
    pname = "opus-wasm32-wasi";
    inherit (pkgs.libopus) version src;
    nativeBuildInputs = [ ghcWasm.all_9_10 ];
    # GHC links the library into a shared one as well.
    env.CFLAGS = "-O2 -fPIC";
    dontDisableStatic = true;
    configureFlags = [
      "--host=wasm32-wasi"
      "CC=wasm32-wasi-clang"
      "LD=wasm-ld"
      "AR=llvm-ar"
      "RANLIB=llvm-ranlib"
      "--disable-shared"
      "--disable-doc"
      "--disable-extra-programs"
      "--disable-stack-protector"
      "--disable-rtcd"
      "--disable-intrinsics"
    ];
  };
in
pkgs.mkShell {
  packages = [ ghcWasm.all_9_10 pkgs.pkg-config ];
  PKG_CONFIG_LIBDIR = "${opus}/lib/pkgconfig";
}

#!/bin/sh
# Builds the page into a directory, from a shell with wasm32-wasi-cabal and
# libopus for wasm32-wasi (see shell.nix).
set -eu

out=$1
here=$(dirname "$0")

wasm32-wasi-cabal build exe:toxav-browser
wasm=$(wasm32-wasi-cabal list-bin exe:toxav-browser)
mkdir -p "$out"
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$wasm" -o "$out/ghc_wasm_jsffi.js"
cp "$wasm" "$out/toxav-browser.wasm"
cp "$here/index.html" "$here/call.js" "$out/"

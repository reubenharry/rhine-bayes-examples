#!/usr/bin/env bash
# Build the WASM reactor module and copy artifacts into web/
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

if [[ -f "$HOME/.ghc-wasm/env" ]]; then
  # shellcheck disable=SC1091
  source "$HOME/.ghc-wasm/env"
fi

if ! command -v wasm32-wasi-cabal >/dev/null 2>&1; then
  echo "wasm32-wasi-cabal not found. Install ghc-wasm-meta and source ~/.ghc-wasm/env" >&2
  exit 1
fi

wasm32-wasi-cabal update
wasm32-wasi-cabal build exe:browser-spike

WASM_BIN="$(wasm32-wasi-cabal list-bin exe:browser-spike)"
cp "$WASM_BIN" web/browser-spike.wasm

POST_LINK="$(wasm32-wasi-ghc --print-libdir)/post-link.mjs"
node "$POST_LINK" -i web/browser-spike.wasm -o web/ghc_wasm_jsffi.js

echo "Built web/browser-spike.wasm and web/ghc_wasm_jsffi.js"
echo "Serve with:  cd web && python3 -m http.server 8080"

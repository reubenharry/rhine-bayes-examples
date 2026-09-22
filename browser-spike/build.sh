#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"
# shellcheck disable=SC1091
[[ -f "$HOME/.ghc-wasm/env" ]] && source "$HOME/.ghc-wasm/env"

if ! command -v wasm32-wasi-cabal >/dev/null; then
  echo "source ~/.ghc-wasm/env first" >&2
  exit 1
fi

# Keep memory use down on first builds
wasm32-wasi-cabal build exe:browser-spike -j1

WASM_BIN="$(wasm32-wasi-cabal list-bin exe:browser-spike)"
cp "$WASM_BIN" web/browser-spike.wasm
POST_LINK="$(wasm32-wasi-ghc --print-libdir)/post-link.mjs"
node "$POST_LINK" -i web/browser-spike.wasm -o web/ghc_wasm_jsffi.js
echo "OK — serve with: cd web && python3 -m http.server 8080"

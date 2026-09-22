# browser-spike

Minimal GHC WASM + canvas demo for this repo.

## What it is

- A dunai `MSF` that maps `(dt, UserInput) -> Picture`
- A Gloss-shaped `Picture` ADT (no OpenGL)
- Canvas painting + mouse/button input via WASM JSFFI
- Static files under `web/` you can open after building

Longer term this runner will grow Rhine + monad-bayes; the spike starts with dunai only.

## Prerequisites

```sh
curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | sh
source ~/.ghc-wasm/env
```

## Build & run

```sh
cd browser-spike
chmod +x build.sh
./build.sh
cd web && python3 -m http.server 8080
```

Open http://localhost:8080 — move the mouse, click **Reset** / **Pulse**.

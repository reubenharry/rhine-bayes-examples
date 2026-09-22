import { WASI, File, OpenFile, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.4.2/dist/index.js";
import createGhcWasmJsffi from "./ghc_wasm_jsffi.js";

const status = document.getElementById("status");

function setStatus(msg) {
  status.textContent = msg;
}

try {
  const wasmResponse = await fetch("./browser-spike.wasm");
  if (!wasmResponse.ok) {
    throw new Error(`Failed to fetch WASM: ${wasmResponse.status}`);
  }
  const wasmBuffer = await wasmResponse.arrayBuffer();

  let exportedFunctions = null;
  const exportsProxy = new Proxy(
    {},
    {
      get(_target, property) {
        if (!exportedFunctions) {
          throw new Error(`WASM exports not ready: ${String(property)}`);
        }
        return exportedFunctions[property];
      },
    }
  );

  const ghcWasmJsffi = createGhcWasmJsffi(exportsProxy);
  const fds = [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered((msg) => console.log(msg)),
    ConsoleStdout.lineBuffered((msg) => console.error(msg)),
  ];
  const wasi = new WASI([], [], fds);

  const { instance } = await WebAssembly.instantiate(wasmBuffer, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghcWasmJsffi,
  });

  exportedFunctions = instance.exports;
  wasi.inst = instance;

  if (typeof instance.exports._initialize === "function") {
    instance.exports._initialize();
  } else if (typeof instance.exports.hs_init === "function") {
    instance.exports.hs_init();
  }

  instance.exports.hs_startDemo();
  setStatus("Running.");
} catch (err) {
  console.error(err);
  setStatus(`Error: ${err.message}`);
}

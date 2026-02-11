// FFI for Test.Crypto.SHA3.Bench
import { performance } from "perf_hooks";

export function performanceNow() {
  return performance.now();
}

// Takes a PureScript thunk (Unit -> a) and runs it in Effect,
// ensuring the computation is performed fresh each time.
export function defer(thunk) {
  return function () {
    return thunk();
  };
}
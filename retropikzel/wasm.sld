(define-library
  (retropikzel wasm)
  (import (scheme base)
          (scheme write)
          (retropikzel leb128))
  (export wasm->sexp)
  (include "wasm.scm"))

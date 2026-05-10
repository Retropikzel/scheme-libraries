(test-begin "wasm")

(define testdir "retropikzel/wasm")
(define testfile1 (string-append testdir "/" "plus.wat"))
(define lib (wat-module->r7rs-library '(wasmtestlibrary) (open-input-file testfile1)))

(with-output-to-file "wasmtestlibrary.sld" (lambda () (show #t (pretty lib))))

(test-end "wasm")

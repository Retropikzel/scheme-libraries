(test-begin "wasm")

(define testdir "retropikzel/wasm")
(define testfile1 (string-append testdir "/" "plus.wasm"))
(define testfile2 (string-append "/tmp/tr7/a.out.wasm"))
(define sexp (wasm->sexp (open-binary-input-file testfile2)))
(show #t (pretty sexp))

(test-end "wasm")

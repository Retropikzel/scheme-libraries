(test-begin "wasm")

(define testdir "retropikzel/wasm")
(define testfile1 (string-append testdir "/" "plus.wasm"))

;(when (not (file-exists? testfile1)) (error (string-append testfile1 " does not exist")))

;(define bytes (with-input-from-file testfile1 (lambda () (read-bytevector 10000))))

(define sexp (with-input-from-file testfile1 (lambda () (wasm->sexp (current-input-port)))))
(write sexp)
(newline)

(test-end "wasm")

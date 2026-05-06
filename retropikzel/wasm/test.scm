(test-begin "wasm")

(define testdir "retropikzel/wasm")
(define testfile1 (string-append testdir "/" "plus.wat"))
(define testfile2 (string-append "/tmp/test/tr7.wat"))
(define lib (wat-module->r7rs-library '(testlibrary) (open-input-file testfile1)))

(with-output-to-file "/tmp/testwasm/testlibrary.sld" (lambda () (show #t (pretty lib))))

(test-end "wasm")

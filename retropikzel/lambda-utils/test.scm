
(test-begin "lambda-utils")

(define dl (default-lambda ((arg1 #f) (arg2 #f)) arg2))

(test-assert (not (dl)))
(test-equal 1 (dl '(arg2 1)))

(define dcl (default-checked-lambda ((arg1 #f string?) (arg2 #f string?)) arg2))

(test-assert (not (dcl)))
(test-equal "hello" (dcl '(arg2 "hello")))

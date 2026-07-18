
(test-runner-current (tap-runner))

(test-begin "tap")
(test-assert #t)
(test-assert #t)

(test-begin "tap 1")
(test-assert #t)
(test-assert #t)
(test-assert #f)
(test-equal '(1 2 3) '(4 5 6))
(test-end "tap 1")

(test-begin "tap 2")
(test-equal '(1 2 3) '(1 2 3))
(test-assert #t)
(test-assert "I have a name" #t)
(test-end "tap 2")

(test-assert #t)
(test-equal '(4 5 6) '(7 8 9))

(test-end "tap")


(test-runner-current (junit-runner))

(test-begin "junit")

(test-begin "junit 1")
(test-assert #t)
(test-assert #t)
(test-assert #f)
(test-equal '(1 2 3) '(4 5 6))
(test-end "junit 1")

(test-begin "junit 2")
(test-equal '(1 2 3) '(1 2 3))
(test-assert #t)
(test-assert "I have a name" #t)
(test-end "junit 2")

(test-end "junit")

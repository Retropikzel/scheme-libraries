(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (srfi 64)
        (retropikzel ctrf))

(test-runner-current (ctrf-runner))

(test-begin "ctrf")

(test-assert #t)
(test-assert #t)
(test-equal '1 '(1 2 3))
(test-assert #t)

(test-end "ctrf")

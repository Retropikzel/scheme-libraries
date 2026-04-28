(define-library
  (retropikzel r7rs-tests)
  (import (scheme base)
          (scheme write)
          (srfi 64)
          (retropikzel ctrf))
  (export run-r7rs-tests)
  (include "r7rs-tests.scm"))

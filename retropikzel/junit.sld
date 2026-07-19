(define-library
  (retropikzel junit)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (srfi 19)
          (srfi 64))
  (export junit-runner)
  (include "junit.scm"))

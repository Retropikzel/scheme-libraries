(define-library
  (retropikzel tap)
  (import (scheme base)
          (scheme write)
          (srfi 64))
  (export tap-runner)
  (include "tap.scm"))


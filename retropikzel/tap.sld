(define-library
  (retropikzel tap)
  (import (scheme base)
          (scheme write)
          (srfi 64))
  (export tap-runner
          set-tap-runner-output-port!)
  (include "tap.scm"))


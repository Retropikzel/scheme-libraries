(define-library
  (retropikzel parallel)
  (import (scheme base)
          (scheme write)
          (scheme eval)
          (retropikzel hardware-info)
          (retropikzel purer))
  (include "parallel/shared.scm")
  (cond-expand
    ((library (srfi 18))
     (import (srfi 18))
     (include "parallel-srfi-18.scm"))
    (else
      (import (scheme base))
      (include "parallel.scm")))
  (export parallel-map))

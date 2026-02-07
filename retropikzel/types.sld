(define-library
  (retropikzel types)
  (cond-expand
    (chicken (import (scheme base)
                     (scheme write)
                     (chicken type)))
    (else
      (import (scheme base)
              (scheme write))))
  (export define-integer
          set-integer!)
  (include "types.scm"))

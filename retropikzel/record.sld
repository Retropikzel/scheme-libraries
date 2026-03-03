(define-library
  (retropikzel record)
  (import (scheme base))
  (export record
          immutable-record
          typed-record)
  (include "record.scm"))

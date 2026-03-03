(define-library
  (retropikzel memoize)
  (import (scheme base)
          (scheme lazy))
  (export memoize)
  (include "memoize.scm"))

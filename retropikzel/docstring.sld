(define-library
  (retropikzel docstring)
  (import (scheme base)
          (scheme write)
          (srfi 39))
  (export doc-string
          doc
          )
  (include "docstring.scm"))


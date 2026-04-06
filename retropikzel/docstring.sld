(define-library
  (retropikzel docstring)
  (import (scheme base)
          (scheme write)
          (srfi 39))
  (export docstring
          doc)
  (include "docstring.scm"))


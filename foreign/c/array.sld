(define-library
  (foreign c array)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme process-context))
  (export make-c-array
          c-array-ref
          c-array-set!
          list->c-array
          c-array->list)
  (include "array.scm"))

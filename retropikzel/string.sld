(define-library
  (retropikzel string)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme process-context))
  (export string-replace
          ;string-replace-times ;; TODO Replace given amount of occurances
          ;; TODO www.w3schools.com/python/python_ref_string.asp
          string-format
          string-capitalize
          string-ends-with?
          string-expand-tabs
          ;; TODO https://dlang.org/library/std/string.html
          ;; TODO https://docs.ruby-lang.org/en/master/String.html
          ;; TODO https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
          )
  (include "string.scm"))

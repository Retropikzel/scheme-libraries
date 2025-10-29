(define-library
  (retropikzel cgi)
  (import (scheme base)
          (scheme time)
          (scheme read)
          (scheme write)
          (scheme file)
          (scheme char)
          (scheme process-context))
  (export cgi
          cgi-exit)
  (include "cgi.scm"))

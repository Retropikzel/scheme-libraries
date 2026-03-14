(define-library
  (retropikzel mouth)
  (import (scheme base)
          (scheme write)
          (scheme file))
  (export slurp
          spit
          ;slurb ; TODO Read whole file as bytevector
          ;sbit ; TODO Write bytevector into file
          )
  (include "mouth.scm"))

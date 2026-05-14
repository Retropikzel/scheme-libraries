(define-library
  (retropikzel tiled)
  (import (scheme base)
          (scheme write)
          (srfi 180))
  (export read-map
          map?)
  (include "tiled.scm"))

(define-library
  (retropikzel polygon)
  (import (scheme base))
  (export make-rectangle-polygon
          make-triangle-polygon
          move-polygon
          rotate-polygon)
  (include "polygon.scm"))

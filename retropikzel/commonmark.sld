(define-library
  (retropikzel commonmark)
  (import (scheme base))
  (export commonmark->sxml
          ;commonmark-file->sxml
          ;commonmark->xml
          ;commonmark-file->xml
          commonmark->html
          ;commonmark-file->html
          )
  (include "commonmark.scm"))


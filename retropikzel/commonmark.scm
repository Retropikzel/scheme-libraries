(define (starts-with? haystack needle)
  (let ((needle-length (string-length needle)))
  (and (>= (string-length haystack) needle-length)
       (string=? (string-copy haystack 0 needle-length) needle))))

(define (internal-loop port result)
  ;; TODO Peak-char and then if it's # for example call read-header
  ;; if it is [ call read-link. The reader functions then either read from port
  ;; if it is a header or link, or do nothing
  #t
  )

(define (commonmark->sxml port)
  (internal-loop port ""))

(define (commonmark->html port)
  "")

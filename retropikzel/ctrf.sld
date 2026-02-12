(define-library
  (retropikzel ctrf)
  (import (scheme base)
          (scheme write)
          (scheme time)
          (scheme file)
          (scheme process-context)
          (srfi 64)
          (srfi 180))
  (export ctrf-runner)
  (begin
    (define operation-system
      (cond-expand
        (windows "windows")
        (linux "linux")
        (guile "linux")
        (else "other"))))
  (cond-expand
    (capyscheme (begin (define implementation-name "capyscheme")))
    (chezscheme (begin (define implementation-name "chezscheme")))
    (chibi (begin (define implementation-name "chibi")))
    (chicken (begin (define implementation-name "chicken")))
    (cyclone (begin (define implementation-name "cyclone")))
    (foment (begin (define implementation-name "foment")))
    (gambit (begin (define implementation-name "gambit")))
    (gauche (begin (define implementation-name "gauche")))
    (guile (begin (define implementation-name "guile")))
    (ikarus (begin (define implementation-name "ikarus")))
    (ironscheme (begin (define implementation-name "ironscheme")))
    (kawa (begin (define implementation-name "kawa")))
    (larceny (begin (define implementation-name "larceny")))
    (loko (begin (define implementation-name "loko")))
    (meevax (begin (define implementation-name "meevax")))
    (mit (begin (define implementation-name "mit-scheme")))
    (mosh (begin (define implementation-name "mosh")))
    (racket (begin (define implementation-name "racket")))
    (sagittarius (begin (define implementation-name "sagittarius")))
    (skint (begin (define implementation-name "skint")))
    (stklos (begin (define implementation-name "stklos")))
    (tr7 (begin (define implementation-name "tr7")))
    (ypsilon (begin (define implementation-name "ypsilon")))
    (else (begin (define implementation-name "unknown"))))
  (cond-expand
    #;(r6rs
      (import (srfi :19))
      (begin
        (define (time-s)
          (time-second (current-time)))))
    #;(srfi-19
      (import (srfi 19))
      (begin
        (define (time-s)
          (time-second (current-time)))))
    (else
      (begin
        (define (time-s) (current-second)))))
  (include "ctrf.scm"))

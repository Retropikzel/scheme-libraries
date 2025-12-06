(define-library
  (retropikzel ctrf)
  (import (scheme base)
            (scheme write)
            (scheme time)
            (scheme process-context)
            (srfi 64)
            (srfi 69)
            (srfi 180))
  (export ctrf-runner)
  (cond-expand
    ;; Guile has both r6rs and r7rs on (features)
    (guile
      (begin (define operation-system
               (cond-expand
                 (windows "windows")
                 (else "unix")))))
    (r6rs
      (begin (define operation-system "unknown")))
    (else
      (begin (define operation-system
               (cond-expand
                 (windows "windows")
                 (linux "linux")
                 (else "other"))))))
  (cond-expand
    (chezscheme (begin (define implementation-name "chezscheme")))
    (chibi (begin (define implementation-name "chibi")))
    (chicken (begin (define implementation-name "chicken")))
    (cyclone (begin (define implementation-name "cyclone")))
    (gambit (begin (define implementation-name "gambit")))
    (gauche (begin (define implementation-name "gauche")))
    (guile (begin (define implementation-name "guile")))
    (ikarus (begin (define implementation-name "ikarus")))
    (ironscheme (begin (define implementation-name "ironscheme")))
    (kawa (begin (define implementation-name "kawa")))
    (mit-scheme (begin (define implementation-name "mit-scheme")))
    (mosh (begin (define implementation-name "mosh")))
    (racket (begin (define implementation-name "racket")))
    (sagittarius (begin (define implementation-name "sagittarius")))
    (stklos (begin (define implementation-name "stklos")))
    (tr7 (begin (define implementation-name "tr7")))
    (ypsilon (begin (define implementation-name "ypsilon")))
    (else (begin (define implementation-name "unknown"))))
  (cond-expand
    (r6rs
      (begin
        (define (time-ms)
          ;; FIXME
          0)))
    (else
      (begin
        (define (time-ms) (/ (/ (current-jiffy) (jiffies-per-second)) 1000)))))
  (include "ctrf.scm"))

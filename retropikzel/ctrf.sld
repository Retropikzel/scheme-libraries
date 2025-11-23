(define-library
  (retropikzel ctrf)
  (import (scheme base)
            (scheme write)
            (scheme time)
            (scheme process-context)
            (srfi 64)
            (srfi 69)
            (srfi 180))
  (export test-begin
          test-end
          test-group
          test-group-with-cleanup
          test-skip
          test-expect-fail
          test-match-name
          test-match-nth
          test-match-all
          test-match-any
          test-assert
          test-eqv
          test-eq
          test-equal
          test-approximate
          test-error
          test-read-eval-string
          test-apply test-with-runner
          test-exit
          test-runner-null
          test-runner?
          test-runner-reset
          test-result-alist
          test-result-alist!
          test-result-ref
          test-result-set!
          test-result-remove
          test-result-clear
          test-runner-pass-count
          test-runner-fail-count
          test-runner-xpass-count
          test-runner-xfail-count
          test-runner-skip-count
          test-runner-test-name
          test-runner-group-path
          test-runner-group-stack
          test-runner-aux-value
          test-runner-aux-value!
          test-result-kind test-passed?
          test-runner-on-test-begin
          test-runner-on-test-begin!
          test-runner-on-test-end
          test-runner-on-test-end!
          test-runner-on-group-begin
          test-runner-on-group-begin!
          test-runner-on-group-end
          test-runner-on-group-end!
          test-runner-on-final
          test-runner-on-final!
          test-runner-on-bad-count
          test-runner-on-bad-count!
          test-runner-on-bad-end-name
          test-runner-on-bad-end-name!
          test-runner-factory
          test-runner-create
          test-runner-current
          test-runner-get
          test-runner-simple
          test-on-group-begin-simple
          test-on-group-end-simple
          test-on-final-simple
          test-on-test-begin-simple
          test-on-test-end-simple
          test-on-bad-count-simple
          test-on-bad-end-name-simple)
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

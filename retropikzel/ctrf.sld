(define-library
  (retropikzel ctrf)
  (cond-expand
    (chezscheme
      (import (rnrs)
              (srfi 64)
              (srfi 69)
              (srfi 180)))
    (else
      (import (scheme base)
              (scheme write)
              (scheme time)
              (scheme process-context)
              (srfi 64)
              (srfi 69)
              (srfi 180))))
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
  (begin
    (define (any->string any)
      (parameterize
        ((current-output-port (open-output-string)))
        (display any)
        (get-output-string (current-output-port))))

    (define runner
      (lambda ()
        (let ((runner (test-runner-null))
              (tests (list))
              (current-test-start-time 0)
              (current-test-groups '()))

          (test-runner-on-group-begin!
            runner
            (lambda (runner suite-name count)
              (set! current-test-groups (append current-test-groups (list suite-name)))))

          (test-runner-on-group-end!
            runner
            (lambda (runner)
              (set! current-test-groups
                (reverse (list-tail (reverse current-test-groups) 1)))))

          (test-runner-on-test-begin!
            runner
            (lambda (runner)
              (set! current-test-start-time (time-ms))))

          (test-runner-on-test-end!
            runner
            (lambda (runner)
              (let* ((name (test-runner-test-name runner))
                     (result (test-result-kind runner))
                     (status (cond ((equal? result 'pass) "passed")
                                   ((equal? result 'xpass) "passed")
                                   ((equal? result 'fail) "failed")
                                   ((equal? result 'xfail) "failed")
                                   ((equal? result 'skipped) "skipped")
                                   (else "other")))
                     (duration (exact (floor (- (time-ms) current-test-start-time))))
                     (extra (make-hash-table)))

                (when (test-result-ref runner 'expected-value)
                  (hash-table-set! extra
                                   'expected-value
                                   (test-result-ref runner 'expected-value)))

                (when (test-result-ref runner 'actual-value)
                  (hash-table-set! extra
                                   'actual-value
                                   (test-result-ref runner 'actual-value)))

                (when (test-result-ref runner 'expected-error)
                  (hash-table-set! extra
                                   'expected-error
                                   (test-result-ref runner 'expected-error)))

                (when (test-result-ref runner 'actual-error)
                  (hash-table-set! extra
                                   'actual-error
                                   (test-result-ref runner 'actual-error)))

                (when (test-result-ref runner 'source-form)
                  (hash-table-set! extra
                                   'source-form
                                   (any->string (test-result-ref runner 'source-form))))

                (let ((test (alist->hash-table
                              `((name . ,name)
                                (status . ,status)
                                (duration . ,duration)
                                (suite . ,current-test-groups)
                                (extra . ,extra)))))

                  (when (test-result-ref runner 'source-file)
                    (hash-table-set! extra
                                     'filePath
                                     (test-result-ref runner 'source-file)))

                  (when (test-result-ref runner 'source-line)
                    (hash-table-set! extra
                                     'line
                                     (test-result-ref runner 'source-line)))

                  (set! tests (append tests (list test)))))))

          (test-runner-on-final!
            runner
            (lambda (runner)
              (let*
                ((pass (test-runner-pass-count runner))
                 (xpass (test-runner-xpass-count runner))
                 (fail (test-runner-fail-count runner))
                 (xfail (test-runner-xfail-count runner))
                 (skipped (test-runner-skip-count runner))
                 (tool (alist->hash-table
                         `((name . "srfi-64-retropikzel-ctrf"))))
                 (summary (alist->hash-table
                            `((tests . ,(+ pass xpass fail xfail))
                              (passed . ,(+ pass xpass))
                              (failed . ,(+ fail xfail))
                              (pending . 0)
                              (skipped . ,skipped)
                              (other . 0))))
                 (results (alist->hash-table
                            `((tool . ,tool)
                              (summary . ,summary)
                              (tests . ,tests))))
                 (env (alist->hash-table
                        `((appName . ,implementation-name)
                          (osPlatform . ,operation-system))))
                 (output (alist->hash-table
                           `((reportFormat . "CTRF")
                             (specVersion . "0.0.0")
                             (results . ,results)
                             (generatedBy . "(retropikzel ctrf)")
                             (environment . ,env)))))

                (display (json-write-string output #t))
                (newline)
                (exit (+ fail xfail)))))
          runner)))
    (test-runner-factory runner)))

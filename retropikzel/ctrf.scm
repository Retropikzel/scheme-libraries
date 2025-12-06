(define (any->string any)
  (parameterize
    ((current-output-port (open-output-string)))
    (display any)
    (get-output-string (current-output-port))))

(define ctrf-runner
  (lambda ()
    (let ((runner (test-runner-null))
          (tests (vector))
          (current-test-start-time 0)
          (current-test-groups (vector)))

      (test-runner-on-group-begin!
        runner
        (lambda (runner suite-name count)
          (set! current-test-groups
            (vector-append current-test-groups (vector suite-name)))))

      (test-runner-on-group-end!
        runner
        (lambda (runner)
          (set! current-test-groups
            (list->vector
              (reverse
                (list-tail
                  (reverse (vector->list current-test-groups))
                  1))))))

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

            (let ((test ;(alist->hash-table
                          `((name . ,name)
                            (status . ,status)
                            (duration . ,duration)
                            (suite . ,current-test-groups)
                            ;(extra . ,extra)
                            )));)

              (when (test-result-ref runner 'source-file)
                (hash-table-set! extra
                                 'filePath
                                 (test-result-ref runner 'source-file)))

              (when (test-result-ref runner 'source-line)
                (hash-table-set! extra
                                 'line
                                 (test-result-ref runner 'source-line)))

              (set! tests (vector-append tests (vector test)))))))

      (test-runner-on-final!
        runner
        (lambda (runner)
          (let*
            ((pass (test-runner-pass-count runner))
             (xpass (test-runner-xpass-count runner))
             (fail (test-runner-fail-count runner))
             (xfail (test-runner-xfail-count runner))
             (skipped (test-runner-skip-count runner))
             (tool `((name . "srfi-64-retropikzel-ctrf")))
             (summary `((tests . ,(+ pass xpass fail xfail))
                        (passed . ,(+ pass xpass))
                        (failed . ,(+ fail xfail))
                        (pending . 0)
                        (skipped . ,skipped)
                        (other . 0)))
             (results `((tool . ,tool)
                        (summary . ,summary)
                        (tests . ,tests)))
             (env `((appName . ,implementation-name)
                    (osPlatform . ,operation-system)))
             (output `((reportFormat . "CTRF")
                       (specVersion . "0.0.0")
                       (results . ,results)
                       (generatedBy . "(retropikzel ctrf)")
                       (environment . ,env))))

            (json-write output (current-output-port))
            (newline)
            (exit (+ fail xfail)))))
      runner)))

(define ctrf-runner
  (lambda ()
    (let ((any->string
            (lambda (any)
              (let ((port (open-output-string)))
                (display any port)
                (newline port)
                (get-output-string port))))
          (runner (test-runner-null))
          (tests (vector))
          (failed-tests (vector))
          (current-test-start-time 0)
          (current-test-groups (vector))
          (current-test-group-count 0)
          (first-group-name #f))

      (test-runner-on-group-begin!
        runner
        (lambda (runner suite-name count)
          (set! current-test-group-count 0)
          (when (not first-group-name) (set! first-group-name suite-name))
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
          (set! current-test-group-count (+ current-test-group-count 1))
          (set! current-test-start-time (time-s))))

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
                 (duration (- (time-s) current-test-start-time))
                 (result-ref
                   (lambda (runner key)
                     (let ((value (test-result-ref runner key)))
                       (if value (any->string value) "")))))
            (let* ((suite (car (reverse (vector->list current-test-groups))))
                   (test `((name . ,name)
                           (status . ,status)
                           (duration . ,duration)
                           (suite . ,suite)
                           (extra .  ((source-file . ,(result-ref runner 'source-file))
                                      (source-line . ,(result-ref runner 'source-line))
                                      (source-form . ,(result-ref runner 'source-form))
                                      (count . ,current-test-group-count)
                                      (expected-value . ,(result-ref runner 'expected-value))
                                      (actual-value . ,(result-ref runner 'actual-value))
                                      (expected-error . ,(result-ref runner 'expected-error))
                                      (actual-error . ,(result-ref runner 'actual-error)))))))

              (when (or (equal? result 'fail)
                        (equal? result 'xfail))
                (let ((failed (cons `(suite . ,suite) test)))
                (display "FAIL " (current-error-port))
                (json-write failed (current-error-port))
                (newline (current-error-port))
                (set! failed-tests (vector-append failed-tests (vector failed)))))
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
             (tool `((name . "(retropikzel ctrf)")
                     (version . "1.0.0")))
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
                       (environment . ,env)))
             (output-file (string-append implementation-name
                                         "-"
                                         first-group-name
                                         ".ctrf.json"))
             (short-output `((scheme . ,implementation-name)
                             (summary . ,summary)
                             (full . ,output-file))))
            (when (file-exists? output-file) (delete-file output-file))
            (with-output-to-file
              output-file
              (lambda ()
                (json-write output (current-output-port))))
           (json-write short-output (current-output-port))
           (newline (current-output-port))
            ;(exit (+ fail xfail))
            )))
      runner)))

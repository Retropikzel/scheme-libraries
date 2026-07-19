(define junit-runner-output-port (current-output-port))
(define (set-junit-runner-output-port! port)
  (set! junit-runner-output-port port))

(define-syntax junit-runner
  (syntax-rules ()
    ((_)
     (letrec* ((hostname (if (file-exists? "/etc/hostname")
                           (with-input-from-file "/etc/hostname"
                                                 (lambda () (read-line)))
                           "localhost"))
               (any->string
                 (lambda (any)
                   (let ((port (open-output-string)))
                     (display any port)
                     (get-output-string port))))
               (indentation '())
               (increase-indentation
                 (lambda ()
                   (set! indentation (append indentation '(#\space #\space)))))
               (decrease-indentation
                 (lambda ()
                   (when (not (null? indentation))
                     (set! indentation (list-tail indentation 2)))))
               (print
                 (lambda args
                   (map (lambda (item)
                          (display item junit-runner-output-port))
                        (append indentation args))))
               (println
                 (lambda args
                   (map (lambda (item)
                          (display item junit-runner-output-port))
                        (append indentation args)) (newline junit-runner-output-port)))
               (string-replace
                 (lambda (str replace with)
                   (list->string
                     (map (lambda (c)
                            (if (char=? c replace)
                              with
                              c))
                          (string->list str)))))
               (runner (test-runner-null))
               (group-tests '())
               (started? #f)
               (current-group-name #f)
               (current-group-start-time #f)
               (current-group-start-date #f)
               (current-test-start-time #f)
               (current-test-groups (vector))
               (current-test-group-count 0)
               (current-suite-name #f)
               (total-pass 0)
               (total-xpass 0)
               (total-fail 0)
               (total-xfail 0)
               (total-skipped 0))

       (test-runner-on-group-begin!
         runner
         (lambda (runner suite-name count)
           (set! current-group-name suite-name)
           (set! current-group-start-time (current-time time-utc))
           (set! current-group-start-date (current-date))
           (set! current-test-group-count 0)
           (set! current-suite-name suite-name)
           (set! current-test-groups
             (vector-append current-test-groups (vector suite-name)))
           (set! group-tests '())))

       (test-runner-on-group-end!
         runner
         (lambda (runner)
           (let* ((group-time
                    (time-second (time-difference (current-time time-utc)
                                                  current-group-start-time)))
                  (pass (- (test-runner-pass-count runner) total-pass))
                  (xpass (- (test-runner-xpass-count runner) total-xpass))
                  (fail (- (test-runner-fail-count runner) total-fail))
                  (xfail (- (test-runner-xfail-count runner) total-xfail))
                  (skipped (- (test-runner-skip-count runner) total-skipped))
                  (total-tests (+ pass xpass fail xfail skipped))
                  (timestamp
                    (date->string current-group-start-date "~1-T~3+0000"))
                  (testsuite-name-raw
                    (apply string-append
                           (map
                             (lambda (item)
                               (string-append item "."))
                             (test-runner-group-path runner))))
                  (testsuite-name
                    (string-replace
                      (string-copy testsuite-name-raw
                                   0
                                   (- (string-length testsuite-name-raw) 1))
                      #\space
                      #\-)))

             (when (> total-tests 0)
               (println "<testsuite"
                        " name=\"" testsuite-name "\""
                        " timestamp=\"" timestamp "\""
                        " hostname=\"" hostname "\""
                        " time=\"" group-time "\""
                        " tests=\"" total-tests "\""
                        " failures=\"" fail "\""
                        " skipped=\"" skipped "\""
                        ">")
               (increase-indentation)
               (for-each
                 (lambda (test)
                   (println "<testcase"
                            " classname=\""
                            (string-append
                              testsuite-name
                              "."
                              (number->string (cdr (assoc 'count test))))
                            "\""
                            " name=\""
                            (if (string=? (cdr (assoc 'name test)) "")
                              (cdr (assoc 'count test))
                              (cdr (assoc 'name test)))
                            "\""
                            " time=\"" (cdr (assoc 'duration test)) "\""
                            ">")
                   (when (string=? (cdr (assoc 'status test)) "failed")
                     (increase-indentation)
                     (println "<failure"
                              " type=\"" "fail" "\""
                              (if (and (not (string=? (cdr (assoc 'expected-value test)) ""))
                                       (not (string=? (cdr (assoc 'actual-value test)) "")))
                                (string-append
                                  " message=\"expected value: "
                                  (cdr (assoc 'expected-value test))
                                  " actual-value: "
                                  (cdr (assoc 'actual-value test))
                                  "\"")
                                "")
                              ">")
                     (println "</failure>")
                     (decrease-indentation))
                   (println "</testcase>"))
                 (reverse group-tests))
               (decrease-indentation)
               (println "</testsuite>")
               (set! current-test-groups
                 (list->vector
                   (reverse
                     (list-tail
                       (reverse (vector->list current-test-groups))
                       1))))
               (set! total-pass (test-runner-pass-count runner))
               (set! total-xpass (test-runner-xpass-count runner))
               (set! total-fail (test-runner-fail-count runner))
               (set! total-xfail (test-runner-xfail-count runner))
               (set! total-skipped (test-runner-skip-count runner))))))

       (test-runner-on-test-begin!
         runner
         (lambda (runner)
           (when (not started?)
             (println "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
             (println "<testsuites>")
             (increase-indentation)
             (set! started? #t))
           (set! current-test-group-count (+ current-test-group-count 1))
           (set! current-test-start-time (current-time time-utc))))

       (test-runner-on-test-end!
         runner
         (lambda (runner)
           (let* ((end-time (current-time time-utc))
                  (duration
                    (time-second
                      (time-difference end-time current-test-start-time)))
                  (name (test-runner-test-name runner))
                  (result (test-result-kind runner))
                  (status (cond ((equal? result 'pass) "passed")
                                ((equal? result 'xpass) "passed")
                                ((equal? result 'fail) "failed")
                                ((equal? result 'xfail) "failed")
                                ((equal? result 'skipped) "skipped")
                                (else "other")))
                  (result-ref
                    (lambda (runner key)
                      (let ((value (test-result-ref runner key)))
                        (if value (any->string value) "")))))
             (let* ((suite (car (reverse (vector->list current-test-groups))))
                    (test `((name . ,name)
                            (status . ,status)
                            (duration . ,duration)
                            (suite . ,suite)
                            (source-file . ,(result-ref runner 'source-file))
                            (source-line . ,(result-ref runner 'source-line))
                            (source-form . ,(result-ref runner 'source-form))
                            (count . ,current-test-group-count)
                            (expected-value . ,(result-ref runner 'expected-value))
                            (actual-value . ,(result-ref runner 'actual-value))
                            (expected-error . ,(result-ref runner 'expected-error))
                            (actual-error . ,(result-ref runner 'actual-error)))))
               (set! group-tests (cons test group-tests))))))

       (test-runner-on-final!
         runner
         (lambda (runner)
           (decrease-indentation)
           (println "</testsuites>")))

       runner))))

(define tap-output-port (current-output-port))
(define (set-tap-runner-output-port! port) (set! tap-output-port port))

(define-syntax tap-runner
  (syntax-rules ()
    ((_)
     (letrec* ((any->string
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
                          (display item tap-output-port))
                        (append indentation args))))
               (println
                 (lambda args
                   (map (lambda (item)
                          (display item tap-output-port))
                        (append indentation args))
                   (display "\n" tap-output-port)))
               (runner (test-runner-null))
               (started? #f)
               (current-test-groups (vector))
               (current-test-group-count 0)
               (current-suite-name #f))

       (test-runner-on-group-begin!
         runner
         (lambda (runner suite-name count)
           (set! current-test-group-count 0)
           (when current-suite-name
             (println "# Subtest: " suite-name)
             (increase-indentation))
           (set! current-suite-name suite-name)
           (set! current-test-groups (vector-append current-test-groups (vector suite-name)))))

       (test-runner-on-group-end!
         runner
         (lambda (runner)
           (when (> current-test-group-count 0)
             (println "1.." current-test-group-count))
           (decrease-indentation)
           (set! current-test-groups
             (list->vector
               (reverse
                 (list-tail
                   (reverse (vector->list current-test-groups))
                   1))))))

       (test-runner-on-test-begin!
         runner
         (lambda (runner)
           (when (not started?)
             (println "TAP version 14")
             (set! started? #t))
           (set! current-test-group-count (+ current-test-group-count 1))))

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
                  (result-ref
                    (lambda (runner key)
                      (let ((value (test-result-ref runner key)))
                        (if value (any->string value) "")))))
             (let* ((failed? (or (equal? result 'fail) (equal? result 'xfail))))
               (when failed? (print "not "))
               (print "ok " current-test-group-count)
               (if (and (string? name) (not (string=? name "")))
                 (println " - " name)
                 (println ""))
               (when failed?
                 (println "  ---")
                 (println "  severity: fail")
                 (println "  data:")
                 (println "    source: " (result-ref runner 'source-form))
                 (println "    got: " (result-ref runner 'actual-value))
                 (println "    expect: " (result-ref runner 'expected-value))
                 (println "  at: ")
                 (println "    file: " (result-ref runner 'source-file))
                 (println "    line: " (result-ref runner 'source-line)))))))
       runner))))

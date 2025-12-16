
(define tests
  (with-input-from-file
    "retropikzel/commonmark/spec.json"
    (lambda ()
      (json-read (current-input-port)))))

(define current-test-group "")

(vector-for-each
  (lambda (test)
    (let ((section (cdr (assoc 'section test)))
          (number (cdr (assoc 'example test)))
          (markdown (cdr (assoc 'markdown test)))
          (html (cdr (assoc 'html test))))
      (when (not (string=? current-test-group section))
        (set! current-test-group section)
        (test-begin section))
      (test-assert (number->string number)
                   (string=? html
                             (commonmark->html (open-input-string markdown))))))
  tests)

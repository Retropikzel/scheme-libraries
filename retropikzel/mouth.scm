(define bite-size 4000)

(define (slurp file-path)
  (when (not (string? file-path)) (error "slurp: file-path must be string" file-path))
  (when (not (file-exists? file-path)) (error "slurp: file-path does not exist" file-path))
  (letrec ((looper (lambda (str result)
                     (if (eof-object? str)
                       result
                       (looper (read-string bite-size) (string-append result str))))))
    (with-input-from-file file-path (lambda () (looper (read-string bite-size) "")))))

(define (spit file-path text . append?)
  (when (not (string? file-path)) (error "spit: file-path must be string" file-path))
  (when (not (string? text)) (error "spit: text must be string" text))
  (when (and (not (null? append?))
             (not (boolean? (car append?))))
    (error "spit: append? must be boolean" (car append?)))
  (let ((content (if (and (not (null? append?)) (equal? (car append?) #t))
                   (slurp file-path)
                   "")))
    (when (file-exists? file-path) (delete-file file-path))
    (with-output-to-file
      file-path
      (lambda ()
        (display content)
        (display text)))))

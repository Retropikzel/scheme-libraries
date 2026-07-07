(define (cpu-count)
  (cond
    ((not (file-exists? "/proc/cpuinfo"))
     (error "Could not get cpu count"))
    (else
      (let ((count 0))
        (letrec*
          ((looper (lambda (line)
                     (cond
                       ((eof-object? line) count)
                       (else
                         (when (and (> (string-length line) 8)
                                    (string=? (string-copy line 0 9) "processor"))
                           (set! count (+ count 1)))
                         (looper (read-line)))))))
          (with-input-from-file
            "/proc/cpuinfo"
            (lambda ()
              (looper (read-line)))))))))

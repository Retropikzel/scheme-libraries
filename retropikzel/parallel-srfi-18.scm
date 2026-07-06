(define thread-count 4)
(define thread-proc (lambda () #t))

(define (new-thread)
  (let ((thread (make-thread
                  (lambda ()
                    (thread-specific-set!
                      (result (apply thread-proc
                                     (thread-specific (current-thread)))))))))
    thread))
(define threads (make-list thread-count (new-thread)))

(define (parallel-map thunk lst)
  (letrec*
    ((lst-length (length lst))
     (result '())
     (looper (index)
        (when (< index lst-length)
  (for-each
    (lambda (thread)
      (when (< (+ index thread-index) lst-length)
        (thread-specific-set! thread (list-ref lst (+ index thread-index)))))
    threads '(0 1 2 3)) ;; FIXME Make thread-index dynamic
  (for-each
    (lambda (thread)
      (when (< (+ index thread-index) lst-length)
        thread-start! thread))
    threads '(0 1 2 3))
  (for-each thread-join! threads)
  (map
    (lambda (thread)
      (thread-specific thread))
    threads '(0 1 2 3))
  )

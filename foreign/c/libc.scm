(define libc-name
  (cond-expand
    (windows "ucrtbase")
    (haiku "root")
    (guile "c")
    (else "c")))
(define-c-library libc
                  '("stdlib.h" "stdio.h" "string.h")
                  libc-name
                  '((additional-versions ("0" "6"))))

(define-c-procedure c-malloc libc 'malloc 'pointer '(int))
(define-c-procedure c-calloc libc 'calloc 'pointer '(int int))
(define-c-procedure c-perror libc 'perror 'void '(pointer))
(define-c-procedure c-free libc 'free 'void '(pointer))
(define-c-procedure c-strlen libc 'strlen 'int '(pointer))
(define-c-procedure c-memset-address->pointer libc 'memset 'pointer '(u64 u8 int))
(define-c-procedure c-memset-pointer->address libc 'memset 'u64 '(pointer u8 int))

(cond-expand
  ;; FIXME
  (ypsilon
   (define (make-c-null) (c-memset-address->pointer 0 0 0))
   (define (c-null? pointer)
     (call-with-current-continuation
       (lambda (k)
         (with-exception-handler
           (lambda (x) (k #f))
           (lambda ()
             (and (c-bytevector? pointer)
                  (= (c-memset-pointer->address pointer 0 0) 0))))))))
  (else))

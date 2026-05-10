(define (get-data wat)
  (car
    (filter-map
      (lambda (item)
        (if (equal? (car item) 'data)
          (let ((bytes
                  (filter-map
                    (lambda (c)
                      (display "HERE: c ")
                      (write c)
                      (newline)
                      (if (char=? c #\null)
                        #f
                        c))
                    (string->list (list-ref item 2)))))
            (display "HERE: bytes")
            (write bytes)
            (newline)
            (display "HERE: something ")
            (write (map (lambda (c) (string->number (string c))) (string->list (list->string bytes))))
            (newline)
            (cons (cadr (list-ref item 1)) bytes))
          #f))
      (cdr wat))))

(define (get-global-export-names wat)
  (filter-map
    (lambda (item)
      (if (and (equal? (car item) 'export) (assq 'global item))
        (cons (cadr (assq 'global item)) (string->symbol (cadr item)))
        #f))
    (cdr wat)))

(define (get-globals wat)
  (filter-map
    (lambda (item)
      (cond
        ((equal? (car item) 'global)
         `(define ,(list-ref item 1) ,(cadr (car (reverse item)))))
        ((and (equal? (car item) 'export) (assq 'global item))
         `(define ,(string->symbol (cadr item)) ,(cadr (car (reverse item)))))
        (else #f)))
    (cdr wat)))

#;(define (get-memory-export-names wat)
(filter-map
  (lambda (item)
    (if (and (equal? (car item) 'export) (assq 'memory item))
      (string->symbol
        (string-append "memory-"
                       (symbol->string (cadr (assq 'memory item)))))
      #f))
  (cdr wat)))

(define (get-memories wat)
  (filter-map
    (lambda (item)
      (cond
        ((equal? (car item) 'memory)
         (let* ((sizes (list-tail item 2))
                (size (* (cond ((= (length sizes) 1)
                                (list-ref sizes 0))
                               (else (list-ref sizes 1)))
                         webassembly-page-size)))
           `(vector-set! memory-index
                         ,(string->number
                            (string-copy (symbol->string (list-ref item 1))
                                         1))
                         (make-bytevector ,size 0))))
        (else #f)))
    (cdr wat)))

(define (get-function-names wat)
  (filter-map
    (lambda (item)
      (if (and (equal? (car item) 'export) (assq 'func item))
        (cons (cadr (assq 'func item)) (string->symbol (cadr item)))
        #f))
    (cdr wat)))

(define (get-functions function-names wat)
  (filter-map
    (lambda (item)
      (if (equal? (car item) 'func)
        (let* ((name (if (assq (list-ref item 1) function-names)
                       (cdr (assq (list-ref item 1) function-names))
                       (list-ref item 1)))
               (params (filter-map
                         (lambda (item)
                           (if (and (list? item) (equal? (car item) 'param))
                             (list-ref item 1)
                             #f))
                         (cdr item)))
               (return (filter-map
                         (lambda (item)
                           (if (and (list? item)
                                    (equal? (car item) 'return))
                             item
                             #f))
                         (cdr item)))
               (body `(call-with-current-continuation
                        (lambda (return)
                          ,@(cdr (filter-map
                                   (lambda (item)
                                     (if (and (list? item)
                                              (equal? (car item) 'param))
                                       #f
                                       (body-item->sexp item)))
                                   (cdr item))))))
               (result `(define ,name
                          (lambda ,params
                            ,body))))
          (if (equal? body '(call-with-current-continuation (lambda (return))))
            #f
            result))
        #f))
    (cdr wat)))

(define (wat-module->r7rs-library library-name port)
  (let* ((wat (read port))
         (data (get-data wat))
         (global-export-names (get-global-export-names wat))
         (globals (get-globals wat))
         ;(memory-export-names (get-memory-export-names wat))
         (memories (get-memories wat))
         (function-names (get-function-names wat))
         (exports (append (map cdr function-names)
                          (map cdr global-export-names)
                          ;memory-export-names
                          ))
         (functions (get-functions function-names wat)))
    `(define-library
       ,library-name
       (import (scheme base)
               (scheme write)
               (only (r6rs bytevectors)
                     endianness
                     bytevector-s32-set!
                     bytevector-s32-ref)
               (srfi 60))
       (export ,@exports)
       (begin
         (define memory-index (make-vector 5243936 (make-bytevector 5243936 0)))
         ,@memories
         (bytevector-copy! (vector-ref memory-index 0) ,(car data) ,(cdr data))
         ,@globals
         ,@functions))))

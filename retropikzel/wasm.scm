(define (get-function-names wat)
  (filter-map
    (lambda (item)
      (if (and (equal? (car item) 'export) (assq 'func item))
        (cons (cadr (assq 'func item)) (string->symbol (cadr item)))
        #f))
    (cdr wat)))

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
                (size (cond ((= (length sizes) 1)
                             (list-ref sizes 0))
                            (else (list-ref sizes 1)))))
         `(vector-set!  memory-index
                        ,(string->number
                           (string-copy (symbol->string (list-ref item 1))
                                        1))
                        (make-vector ,size 0))))
        (else #f)))
    (cdr wat)))

(define (symbol-append sym1 sym2)
  (string->symbol (string-append (symbol->string sym1) (symbol->string sym2))))

(define (char-index str c)
  (letrec*
    ((looper
       (lambda (index)
         (cond ((>= index (string-length str)) -1)
               ((char=? c (string-ref str index)) index)
               (else (looper (+ index 1)))))))
    (looper 0)))

(define (offset=N->N sym)
  (string->number
    (string-copy
      (symbol->string sym)
      (+ (char-index (symbol->string sym) #\=) 1))))

(define (symbol-starts-with? sym str)
  (if (not (symbol? sym))
    #f
    (let ((sym-str (symbol->string sym)))
      (and (>= (string-length sym-str) (string-length str))
           (string=? (string-copy sym-str 0 (string-length str)) str)))))

(define (body-item->sexp item)
  (cond ((not (list? item)) item)
        ((equal? (car item) 'local)
         `(define ,(list-ref item 1) 0))
        ((equal? (car item) 'global.get)
         (list-ref item 1))
        ((equal? (car item) 'global.set)
         `(set! ,(list-ref item 1) ,@(map body-item->sexp (list-tail item 2))))
        ((equal? (car item) 'global.tee)
         `(set! ,(list-ref item 1) ,@(map body-item->sexp (list-tail item 2))))
        ((equal? (car item) 'local.get)
         (list-ref item 1))
        ((equal? (car item) 'local.set)
         `(set! ,(list-ref item 1) ,@(map body-item->sexp (list-tail item 2))))
        ((equal? (car item) 'local.tee)
         `(set! ,(list-ref item 1) ,@(map body-item->sexp (list-tail item 2))))
        ((equal? (car item) 'result)
         #f)
        ((equal? (car item) 'i32.const)
         (list-ref item 1))
        ((equal? (car item) 'i32.and)
         `(bitwise-and ,@(map body-item->sexp (cdr item))))
        ((equal? (car item) 'i32.add)
         `(+ ,@(map body-item->sexp (cdr item))))
        ((equal? (car item) 'i32.sub)
         `(- ,@(map body-item->sexp (cdr item))))
        ((equal? (car item) 'i32.store)
         `(vector-set!
            (vector-ref memory-index
                        ,(if (list? (list-ref item 2))
                           (body-item->sexp (list-ref item 2))
                           (list-ref item 2)))
            ,(offset=N->N (body-item->sexp (list-ref item 1)))
            ,(body-item->sexp (list-ref item 3))))
        ((equal? (car item) 'i32.load)
         `(vector-ref
            (vector-ref memory-index
                        ,(if (symbol-starts-with? (list-ref item 1) "offset=")
                           (offset=N->N (list-ref item 1))
                           0))
            ,(if (symbol-starts-with? (list-ref item 1) "offset=")
               (if (list? (list-ref item 2))
                 (body-item->sexp (list-ref item 2))
                 (body-item->sexp (list-ref item 2)))
               (if (list? (list-ref item 1))
                 (body-item->sexp (list-ref item 1))
                 (body-item->sexp (list-ref item 1))))))
        ((equal? (car item) 'call)
         `(,@(map body-item->sexp (cdr item))))
        #;((equal? (car item) 'block)
         (display "HERE: block ")
         (write (list-tail item 2))
         (newline)
         `(call-with-current-continuation
            (lambda (,(list-ref item 1))
              ,@(map body-item->sexp (list-tail item 2)))))
        #;((or (equal? (car item) 'i32.eqz))
         `(= ,@(map body-item->sexp (cdr item))))
        #;((equal? (car item) 'br_if)
         `(when (= 1 ,@(map body-item->sexp (list-tail item 2))) (,(list-ref item 1))))
        (else (map body-item->sexp item)))
  )

(define (get-functions function-names wat)
  (filter-map
    (lambda (item)
      (if (equal? (car item) 'func)
        (let ((name (if (assq (list-ref item 1) function-names)
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
              (body (filter-map
                      (lambda (item)
                        (if (and (list? item)
                                 (equal? (car item) 'param))
                          #f
                          (body-item->sexp item)))
                      (cdr item))))
          (list-set! item 0 'define)
          (list-set! item 1 name)
          `(define ,name
             (lambda ,params
               (call-with-current-continuation
                 (lambda (return)
                   ,@(cdr body))))))
        #f))
    (cdr wat)))

(define (wat-module->r7rs-library library-name port)
  (let* ((wat (read port))
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
               (srfi 60))
       (export ,@exports)
       (begin
         (define memory-index (make-vector 8))
              ,@memories
              ,@globals
              ,@functions))))

(define (read-bytevector-until until . port)
  (letrec*
    ((current-port (if (null? port) (current-input-port) (car port)))
     (checklist (if (list? until) until (list until)))
     (looper (lambda (bytes)
               (display "HERE: ")
               (write checklist)
               (newline)
               (write (member (peek-u8 current-port) checklist =))
               (newline)
               (if (or (eof-object? (peek-u8 current-port))
                       (member (peek-u8 current-port) checklist =))
                 bytes
                 (looper (bytevector-append bytes (bytevector (read-u8 current-port))))))))
    (looper (bytevector))))

(define (list-slice l start end)
  (vector->list (vector-copy (list->vector l) start end)))

(define (byte->type byte)
  (cond ((= byte 0) 'void)
        ;; Number types
        ((= byte #x7C) 'f64)
        ((= byte #x7D) 'f32)
        ((= byte #x7E) 'i64)
        ((= byte #x7F) 'i32)
        ;; Vector type
        ((= byte #x7B) 'V128)
        ;; Heap types
        ((= byte #x69) 'exn)
        ((= byte #x6A) 'array)
        ((= byte #x6B) 'struct)
        ((= byte #x6C) 'i31)
        ((= byte #x6D) 'eq)
        ((= byte #x6E) 'any)
        ((= byte #x6F) 'extern)
        ((= byte #x70) 'func)
        ((= byte #x71) 'none)
        ((= byte #x72) 'noextern)
        ((= byte #x73) 'nofunc)
        ((= byte #x74) 'noexn)
        (else
          (display "byte->type warning: Unknown type ")
          (display (bytevector byte))
          (newline)
          'unknown)))

(define (read-types type-count section-size port result)
  (letrec*
    ((type-delimiter #x60)
     (type-signature-byte->name
       (lambda (byte)
         (cond ((= byte #x5E) 'array)
               ((= byte #x5F) 'struct)
               ((= byte #x60) 'func)
               (else 'unknown))))
     (type-signature-byte?
       (lambda (byte)
         (not (symbol=? (type-signature-byte->name byte) 'unknown))))
     (type-bytes->type
       (lambda (bytes)
         (let*
           ((argument-count (list-ref bytes 1))
            (argument-types (map byte->type (list-slice bytes 2 (+ 2 argument-count))))
            (return-type (byte->type (car (reverse bytes)))))
           (append
             `(,(type-signature-byte->name (list-ref bytes 0))
                ,(if (null? argument-types) '() `(param ,@argument-types))
                ,(if (symbol=? return-type 'void) '() `(result ,return-type)))))))
     (read-type
       (lambda (type-result)
         (cond
           ((and (or (eof-object? (peek-u8 port))
                     (type-signature-byte? (peek-u8 port))
                     (= (+ (apply + (map length result))
                           (+ (length type-result) 1))
                        section-size))
                 (> (length type-result) 0))
            (reverse type-result))
           (else
             (read-type (cons (read-u8 port) type-result)))))))
    (cond
      ((= (length result) type-count)
       (map type-bytes->type (reverse result)))
      (else (read-types type-count section-size port (cons (read-type '()) result))))))

(define (read-import import-count section-size port result)
  (if (= (length result) import-count)
    result
    (letrec*
      ((import-kind->type (lambda (type) (cond ((= type 0) 'func) (else 'unknown))))
       (module-name-length (read-u8 port))
       (module-name (utf8->string (read-bytevector module-name-length port)))
       (field-name-length (read-u8 port))
       (field-name (utf8->string (read-bytevector field-name-length port)))
       (import-kind (read-u8 port))
       (type-index (read-u8 port)))
      (read-import import-count
                   section-size
                   port
                   (cons `(import (module ,module-name)
                                  (field ,field-name)
                                  (type ,type-index))
                         result)))))

(define (read-function function-count section-size port result)
  (display "HERE: ")
  (write result)
  (newline)
  (if (= (length result) function-count)
    (reverse result)
    (read-function function-count
                   section-size
                   port
                   (cons (read-bytevector-until 0 port) result))))

(define (read-tables type-count section-size port result)
  `((bytes ,(read-bytevector (- section-size 1) port))))

(define (read-memory type-count section-size port result)
  `((bytes ,(read-bytevector (- section-size 1) port))))

(define (read-global type-count section-size port result)
  `((bytes ,(read-bytevector (- section-size 1) port))))

(define (read-section name size port)
  (list name
        (cond
          ((symbol=? name 'custom) (read-bytevector size port))
          ((symbol=? name 'type) (read-types (read-u8 port) size port '()))
          ((symbol=? name 'import) (read-import (read-u8 port) size port '()))
          ((symbol=? name 'function) (read-function (read-u8 port) size port '()))
          ((symbol=? name 'table) (read-tables (read-u8 port) size port '()))
          ((symbol=? name 'memory) (read-memory (read-u8 port) size port '()))
          ((symbol=? name 'global) (read-global (read-u8 port) size port '()))
          ((symbol=? name 'export) (read-bytevector size port))
          ((symbol=? name 'start) (read-bytevector size port))
          ((symbol=? name 'element) (read-bytevector size port))
          ((symbol=? name 'code) (read-bytevector size port))
          ((symbol=? name 'data) (read-bytevector size port)))))

(define (section-id->name id)
  (cond ((= id 0) 'custom)
        ((= id 1) 'type)
        ((= id 2) 'import)
        ((= id 3) 'function)
        ((= id 4) 'table)
        ((= id 5) 'memory)
        ((= id 6) 'global)
        ((= id 7) 'export)
        ((= id 8) 'start)
        ((= id 9) 'element)
        ((= id 10) 'code)
        ((= id 11) 'data)
        ((= id 12) 'data-count)
        ((= id 13) 'tag)
        (else (error "section-id->name: unrecognized section id" id))))

(define (wasm->sexp-loop port result)
  (let* ((section-id (read-u8 port))
         (section-name (section-id->name section-id))
         (size (read-uleb128 port))
         (section (read-section section-name size port)))
    (display "Section name: ")
    (display section-name)
    (newline)
    (display "Section size: ")
    (write size)
    (newline)
    (if (eof-object? (peek-u8 port))
      (reverse result)
      (wasm->sexp-loop port (cons section result)))))

(define (wasm->sexp port)
  (let ((magic-bytes (read-bytevector 4 port)))
    (when (not (equal? magic-bytes #u8(#x00 #x61 #x73 #x6D)))
      (error "Binary is not wasm (missing magic bytes)"))
    (letrec*
      ((version (read-bytevector 4 port)))
      (wasm->sexp-loop port '()))))


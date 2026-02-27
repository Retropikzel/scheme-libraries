(define (wasm-bytevector? bytes)
  (equal? (bytevector-copy bytes 0 4) (bytevector #x0 #x61 #x73 #x6D)))

(define (wasm-version bytes)
  (bytevector-copy bytes 4 8))

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

(define (bytes->type bytes)
  (let ((first-byte (bytevector-u8-ref bytes 0)))
    (display "HERE: ")
    (display first-byte)
    (newline)
    (cond
      ((equal? first-byte #x7C) 'f64)
      ((equal? first-byte #x7D) 'f32)
      ((equal? first-byte #x7E) 'f64)
      ((equal? first-byte #x7F) 'f32)
      (else 'unknown)
      ;(else (error "Unsupported type byte" first-byte))
      )))

(define (bytes->types bytes)
  (letrec*
    ((bytes-length (bytevector-length bytes))
     (looper
       (lambda (index types)
         (if (>= index bytes-length)
           types
           (looper (+ index 1) (bytes->type (bytevector-copy bytes index)))))))
    (looper 0 '())))

(define (section-bytes->sexp name bytes)
  (cond
    ((equal? name 'type)
     (let* ((number-of-types (leb128->integer-and-length bytes))
            (types (bytes->types (bytevector-copy bytes (cdr number-of-types)))))
       (display "TYPE: ")
       (write bytes)
       (newline)
       `((number-of-types . ,(car number-of-types))
         (types . ,types)
         )))
    (else '())))

(define (wasm->sexp bytes)
  (display bytes)
  (newline)
  (letrec*
    ((bytes-length (bytevector-length bytes))
     (section-data '())
     (index 8) ;; Jump over magic bytes and version
     (looper (lambda ()
               (when (< index bytes-length)
                 (let* ((id (bytevector-u8-ref bytes index))
                        (name (section-id->name id))
                        (len (uleb128->integer-and-length bytes (+ index 1) 0 0))
                        (data-bytes (bytevector-copy bytes index (+ index (car len)))))
                   (set! section-data
                     (append section-data
                             `((id . ,id)
                               (name . ,name)
                               (data . ,(section-bytes->sexp name data-bytes)))))
                   (set! index (+ index 1 (car len) (cdr len)))
                   (looper)
                   )))))
    (looper)
     section-data
    ))

#;(define-record-type <c-struct>
  (c-struct-make c-type size pointer members)
  c-struct?
  (c-type c-struct:type)
  (size c-struct:size)
  (pointer c-struct:pointer)
  (members c-struct:members))

(define round-to-next-modulo-of
  (lambda (to-round roundee)
    (if (= (modulo to-round roundee) 0)
      to-round
      (round-to-next-modulo-of (+ to-round 1) roundee))))

(define calculate-struct-members
  (lambda (members)
    (let*
      ((size 0)
       (largest-member-size 0)
       (data (map (lambda (member)
                    (let* ((name (list-ref member 0))
                           (type (list-ref member 1))
                           (accessor (list-ref member 2))
                           (type-alignment (c-type-align type)))
                      (when (> (size-of-type type) largest-member-size)
                        (set! largest-member-size (size-of-type type)))
                      (if (or (= size 0)
                              (= (modulo size type-alignment) 0))
                        (begin
                          (set! size (+ size type-alignment))
                          (list name type (- size type-alignment) accessor))
                        (let ((next-alignment
                                (round-to-next-modulo-of size type-alignment)))
                          (set! size (+ next-alignment type-alignment))
                          (list name type next-alignment accessor)))))
                  members)))
      data)))


(define-syntax define-c-struct
  (syntax-rules ()
    ((_ name members struct-pointer (field-name field-type accessor modifier) ...)
     (begin
       (define accessor
         (lambda (c-bytevector)
           (let ((offset (let ((offset 0)
                               (before? #t))
                           (for-each
                             (lambda (member)
                               (when (equal? (list-ref member 0) 'field-name)
                                 (set! before? #f))
                               (when before?
                                 (set! offset
                                   (+ offset
                                      (c-type-align (list-ref member 1))))))
                             members)
                           offset)))
             (cond
               ((equal? 'pointer field-type)
                (c-bytevector-pointer-ref c-bytevector offset))
               ((c-type-signed? field-type)
                (c-bytevector-sint-ref c-bytevector
                                       offset
                                       (native-endianness)
                                       (c-type-size field-type)))
               (else
                 (c-bytevector-uint-ref c-bytevector
                                        offset
                                        (native-endianness)
                                        (c-type-size field-type)))))))
       ...
       (define modifier
         (lambda (c-bytevector value)
           (let ((offset (let ((offset 0)
                               (before? #t))
                           (for-each
                             (lambda (member)
                               (when (equal? (list-ref member 0) 'field-name)
                                 (set! before? #f))
                               (when before?
                                 (set! offset
                                   (+ offset
                                      (c-type-align (list-ref member 1))))))
                             members)
                           offset)))
             (cond
               ((equal? 'pointer field-type)
                (c-bytevector-pointer-set! c-bytevector offset value))
               ((c-type-signed? field-type)
                (c-bytevector-sint-set! c-bytevector
                                        offset
                                        value
                                        (native-endianness)
                                        (c-type-size field-type)))
               (else
                 (c-bytevector-uint-set! c-bytevector
                                         offset
                                         value
                                         (native-endianness)
                                         (c-type-size field-type)))))))
       ...
       (define members (calculate-struct-members
                         (list (list 'field-name field-type accessor) ...)))
       (define name
         (if (c-null? struct-pointer)
           (make-c-bytevector (+ (c-type-size field-type) ...))
           struct-pointer))))))

(define c-struct->alist
  (lambda (struct-c-bytevector struct-members)
    (map (lambda (member)
           (cons (list-ref member 0)
                 (apply (list-ref member 3) (list struct-c-bytevector))))
         struct-members)))

#;(define-syntax define-c-struct
  (syntax-rules ()
    ((_ name constructor pred field ...)
     (define name
       (lambda arguments
         (let* ((size-and-offsets (calculate-struct-size-and-offsets members))
                (size (cdr (assoc 'size size-and-offsets)))
                (offsets (cdr (assoc 'offsets size-and-offsets)))
                (pointer (if (and (not (null? arguments))
                                  (c-bytevector? (car arguments)))
                           (car arguments)
                           (make-c-bytevector size)))
                (c-type-string (if (string? c-type) c-type (symbol->string c-type))))
           (c-struct-make c-type-string size pointer offsets)))))))

#;(define pffi-struct-make
  (lambda (c-type members . pointer)
  (for-each
    (lambda (member)
      (when (not (pair? member))
        (error "All struct members must be pairs" (list c-type member)))
      (when (not (symbol? (car member)))
        (error "All struct member types must be symbols" (list c-type member)))
      (when (not (symbol? (cdr member)))
        (error "All struct member names must be symbols" (list c-type member))))
    members)
  (let* ((size-and-offsets (calculate-struct-size-and-offsets members))
         (size (cdr (assoc 'size size-and-offsets)))
         (offsets (cdr (assoc 'offsets size-and-offsets)))
         (pointer (if (null? pointer) (make-c-bytevector size) (car pointer)))
         (c-type (if (string? c-type) c-type (symbol->string c-type))))
    (struct-make c-type size pointer offsets))))

#;(define (pffi-struct-offset-get struct member-name)
  (when (not (assoc member-name (pffi-struct-members struct)))
    (error "Struct has no such member" (list struct member-name)))
  (car (cdr (cdr (assoc member-name (pffi-struct-members struct))))))

#;(define (pffi-struct-get struct member-name)
  (when (not (assoc member-name (pffi-struct-members struct)))
    (error "Struct has no such member" (list struct member-name)))
  (let ((type (car (cdr (assoc member-name (pffi-struct-members struct)))))
        (offset (car (cdr (cdr (assoc member-name (pffi-struct-members struct)))))))
    (pffi-pointer-get (pffi-struct-pointer struct) type offset)))

#;(define (pffi-struct-set! struct member-name value)
  (when (not (assoc member-name (pffi-struct-members struct)))
    (error "Struct has no such member" (list struct member-name)))
  (let ((type (car (cdr (assoc member-name (pffi-struct-members struct)))))
        (offset (car (cdr (cdr (assoc member-name (pffi-struct-members struct)))))))
    (pffi-pointer-set! (pffi-struct-pointer struct) type offset value)))

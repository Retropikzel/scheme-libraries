(define make-c-array
  (lambda (type size . fill)
    (let ((array (make-c-bytevector (* (c-type-size type) size))))
      (when (not (null? fill))
        (letrec* ((filler (car fill))
                  (looper (lambda (count)
                          (when (> size count)
                            (c-array-set! array type count filler)
                            (looper (+ count 1))))))
          (looper 0)))
      array)))

(define c-array-ref
  (lambda (array type index)
    (let* ((size (c-type-size type))
           (offset (* index size)))
      (cond
        ((equal? 'pointer type)
         (c-bytevector-pointer-ref array offset))
        ((c-type-signed? type)
         (c-bytevector-sint-ref array offset (native-endianness) size))
        (else
          (c-bytevector-uint-ref array offset (native-endianness) size))))))

(define c-array-set!
  (lambda (array type index value)
    (let* ((size (c-type-size type))
           (offset (* index size)))
      (cond
        ((equal? 'pointer type)
         (c-bytevector-pointer-set! array offset value))
        ((c-type-signed? type)
         (c-bytevector-sint-set! array offset value (native-endianness) size))
        (else
          (c-bytevector-uint-set! array offset value (native-endianness) size))))))

(define list->c-array
  (lambda (list type)
    (let* ((array-size (length list))
           (type-size (c-type-size type))
           (array (make-c-bytevector (* type-size array-size)))
           (index 0))
      (for-each
        (lambda (item)
          (c-array-set! array type index item)
          (set! index (+ index 1)))
        list)
      array)))

(define c-array->list
  (lambda (array type size)
    (letrec*
      ((looper (lambda (index result)
                 (if (>= index size)
                   result
                   (looper (+ index 1)
                           (append result
                                   (list (c-array-ref array type index))))))))
      (looper 0 (list)))))

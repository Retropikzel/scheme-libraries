(define (memoize thunk)
  (force (delay (apply thunk '()))))

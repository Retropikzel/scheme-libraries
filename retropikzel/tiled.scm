(define-record-type <map>
  (make-map width height layers)
  map?
  (width map-width)
  (height map-height)
  (layers map-layers))


(define (read-map port)
  (json-read port)
  )

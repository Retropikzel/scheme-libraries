
(test-begin "tiled")

(define mapdata (read-map (open-input-file "retropikzel/tiled/testmap.json")))

(write mapdata)
(newline)

(test-end "tiled")

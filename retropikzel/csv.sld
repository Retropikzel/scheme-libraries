(define-library
  (retropikzel csv)
  (import
    (scheme base)
    (scheme read)
    (scheme char)
    (scheme write)
    (scheme file))
  (export csv->list csv-from-list csv-file->list)
  (include "csv.scm"))

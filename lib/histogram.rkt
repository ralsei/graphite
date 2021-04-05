#lang racket
(require plot/pict "util.rkt")
(provide histogram)

(define ((histogram #:bins [bins 30] #:mapping [local-mapping (make-hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define xs
    (for/vector ([(x facet) (in-data-frame* (gr-data) (hash-ref aes 'x)
                                            (hash-ref aes 'facet #f))]
                 #:when x
                 #:when (equal? facet (gr-group)))
      ((gr-x-conv) x)))

  (define min-data (for/fold ([m +inf.0]) ([x (in-vector xs)]) (min m x)))
  (define max-data (for/fold ([m -inf.0]) ([x (in-vector xs)]) (max m x)))

  ; NOTE: in-range is exclusive, hence the add1. we subtract bin-width as
  ; we're continually adding bin-width in the #:when.
  (define bin-width (/ (- max-data min-data) (add1 bins)))
  (define count-tbl (make-hash))
  (for* ([i (in-range min-data (- max-data bin-width) bin-width)]
         [x (in-vector xs)]
         #:when (<= i x (+ i bin-width)))
    (hash-update! count-tbl i add1 1))

  (run-renderer #:renderer rectangles #:mapping aes
                (for/vector ([(k v) (in-hash count-tbl)])
                  (vector (ivl k (+ bin-width k)) (ivl 0 v)))))

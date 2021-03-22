#lang racket
(require plot/pict "util.rkt")
(provide histogram)

(define ((histogram #:bins [bins 30] #:mapping [local-mapping (make-hash)])
         #:data data #:gmapping mapping #:x-conv x-conv #:y-conv y-conv #:group group)
  (define aes (mapping-override mapping local-mapping))
  (define xs
    (for/vector ([(x facet) (in-data-frame* data (hash-ref aes 'x)
                                            (hash-ref aes 'facet #f))]
                 #:when x
                 #:when (if group (equal? facet group) #t))
      (x-conv x)))

  (define min-data (for/fold ([m +inf.0]) ([x (in-vector xs)]) (min m x)))
  (define max-data (for/fold ([m -inf.0]) ([x (in-vector xs)]) (max m x)))
  (define bin-width (/ (- max-data min-data) bins))

  (define count-tbl (make-hash))
  (for* ([i (in-range min-data max-data bin-width)]
         [x (in-vector xs)]
         #:when (<= i x (+ i bin-width)))
    (hash-update! count-tbl i add1 1))

  (rectangles
   (for/vector ([(k v) (in-hash count-tbl)])
     (vector (ivl k (+ bin-width k)) (ivl 0 v)))))

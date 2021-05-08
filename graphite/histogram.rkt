#lang racket
(require fancy-app plot/pict plot/utils pict
         "util.rkt" "contracts.rkt")
(provide
 (contract-out [histogram (->* ()
                               (#:bins positive-integer?
                                #:mapping (aes-containing/c #:x string?
                                                            #:y string?
                                                            #:facet (or/c string? #f)
                                                            #:x-min (or/c rational? #f)
                                                            #:x-max (or/c rational? #f)
                                                            #:y-min (or/c rational? #f)
                                                            #:y-max (or/c rational? #f)
                                                            #:color plot-color/c
                                                            #:style plot-brush-style/c
                                                            #:line-color plot-color/c
                                                            #:line-width (>=/c 0)
                                                            #:line-style plot-pen-style/c
                                                            #:alpha (real-in 0 1)
                                                            #:label (or/c string? pict? #f)))
                               graphite-renderer?)]))

(define (mean lst)
  (/ (foldr + 0 lst) (length lst)))

(define ((histogram #:bins [bins 30] #:mapping [local-mapping (make-hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))

  (define-values (xs ys)
    (for/lists (l1 l2)
               ([(x y facet) (in-data-frame* (gr-data) (hash-ref aes 'x)
                                             (hash-ref aes 'y #f)
                                             (hash-ref aes 'facet #f))]
                #:when x
                #:when (equal? facet (gr-group)))
      (values ((gr-x-conv) x) ((gr-y-conv) y))))

  (define min-data (for/fold ([m +inf.0]) ([x (in-list xs)]) (min m x)))
  (define max-data (for/fold ([m -inf.0]) ([x (in-list xs)]) (max m x)))

  ; NOTE: in-range is exclusive, hence the add1. we subtract bin-width as
  ; we're continually adding bin-width in the #:when.
  (define bin-width (/ (- max-data min-data) (add1 bins)))
  (define binned (make-hash))
  (for* ([i (in-range min-data (- max-data bin-width) bin-width)]
         [(x y) (in-parallel xs ys)]
         #:when (<= i x (+ i bin-width)))
    (hash-update! binned i (cons y _) null))

  (define to-plot
    (for/hash ([(i ys) (in-hash binned)])
      (values i (cond [(hash-ref aes 'y #f) (mean ys)]
                      [else (length ys)]))))

  (run-renderer #:renderer rectangles #:mapping aes
                (for/vector ([(k v) (in-hash to-plot)])
                  (vector (ivl k (+ bin-width k)) (ivl 0 v)))))

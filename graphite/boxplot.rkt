#lang racket
(require fancy-app plot/pict plot/utils
         "extern/box-and-whiskers.rkt" "util.rkt")
(provide boxplot)

(define (make-stat-table mapping iqr-scale)
  (define list-tbl (make-hash))
  (for ([(x y facet) (in-data-frame* (gr-data) (hash-ref mapping 'x)
                                     (hash-ref mapping 'y)
                                     (hash-ref mapping 'facet #f))]
        #:when (and x y)
        #:when (equal? facet (gr-group)))
    (hash-update! list-tbl ((gr-x-conv) x) (cons ((gr-y-conv) y) _) '()))


  (for/hash ([(k v) (in-hash list-tbl)])
    (values k (samples->bnw-data v #:iqr-scale iqr-scale))))

(define ((boxplot #:iqr-scale [iqr-scale 1.5] #:mapping [local-mapping (make-hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))

  (for/list ([(k v) (in-hash (make-stat-table aes iqr-scale))]
             [c (in-naturals)])
    (displayln c)
    (run-renderer #:renderer box-and-whiskers
                  #:mapping aes
                  #:x c
                  v)))

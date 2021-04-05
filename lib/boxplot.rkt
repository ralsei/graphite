#lang racket
(require fancy-app math/statistics plot/pict plot/utils "util.rkt")

(define (five-number-summary lst)
  (values (apply min lst)
          (quantile 0.25 < lst)
          (median lst)
          (quantile 0.75 < lst)
          (apply max lst)))

(define (make-stat-table)
  (define list-tbl (make-hash))
  (for ([(x y facet) (in-data-frame* (gr-data) (hash-ref (gr-global-mapping) 'x)
                                     (hash-ref (gr-global-mapping) 'y)
                                     (hash-ref (gr-global-mapping) 'facet #f))]
        #:when (and x y)
        #:when (equal? facet (gr-group)))
    (hash-update! list-tbl ((gr-x-conv) x) (cons ((gr-y-conv) y) _) '()))

  ; ... some stuff ...
  list-tbl)

(define ((boxplot #:mapping [local-mapping (make-hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))

  3)

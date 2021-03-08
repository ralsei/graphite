#lang racket
(require racket/hash
         data-frame fancy-app plot/pict plot/utils)
(provide ppoints)

(define ((ppoints #:mapping [local-mapping (make-hash)])
         #:data data #:gmapping mapping #:x-conv x-conv #:y-conv y-conv)
  (define aes (hash-union mapping local-mapping #:combine (λ (x y) x)))
  (define discrete-color (hash-ref aes 'discrete-color #f))
  (define alpha (hash-ref aes 'alpha 1))

  (define facet #f)
  (define facet-predicate (lambda _ #t))
  
  (define tbl (make-hash))
  (define disc-color (if discrete-color
                         (in-data-frame data discrete-color)
                         (in-cycle (in-value #f))))
  (define facets (if facet
                    (apply in-data-frame/list data facet)
                    (in-sequence-forever null null)))
  (for ([(x y) (in-data-frame data (hash-ref aes 'x) (hash-ref aes 'y))]
        [strat disc-color]
        [facets facets]
        #:when (and x y)
        #:when (apply facet-predicate facets))
    (hash-update! tbl strat (cons (vector (x-conv x) (y-conv y)) _) null))
  
  (let ([color-n -1])
    (hash-map tbl
              (λ (strat pts)
                (set! color-n (add1 color-n))
                (points pts #:color (->pen-color color-n) #:label strat #:alpha alpha))
              #t)))

#lang racket
(require data-frame fancy-app plot/pict plot/utils
         "util.rkt")
(provide ppoints)

(define ((ppoints #:mapping [local-mapping (make-hash)])
         #:data data #:gmapping mapping #:x-conv x-conv #:y-conv y-conv #:group group)
  (define aes (mapping-override mapping local-mapping))
  (define discrete-color (hash-ref aes 'discrete-color #f))
  (define alpha (hash-ref aes 'alpha 1))
  
  (define tbl (make-hash))
  (define disc-color (if discrete-color
                         (in-data-frame data discrete-color)
                         (in-infinite #f)))
  (define facets (if (hash-ref aes 'facet #f)
                     (in-data-frame data (hash-ref aes 'facet))
                     (in-infinite null)))

  (for ([(x y) (in-data-frame data (hash-ref aes 'x) (hash-ref aes 'y))]
        [strat disc-color]
        [facet facets]
        #:when (and x y)
        #:when (if group (equal? facet group) #t))
    (hash-update! tbl strat (cons (vector (x-conv x) (y-conv y)) _) null))
  
  (let ([color-n -1])
    (hash-map tbl
              (λ (strat pts)
                (set! color-n (add1 color-n))
                (points pts #:color (->pen-color color-n) #:label strat #:alpha alpha))
              #t)))

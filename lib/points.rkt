#lang racket
(require fancy-app plot/pict plot/utils
         "util.rkt")
(provide ppoints)

(define ((ppoints #:mapping [local-mapping (make-hash)])
         #:data data #:gmapping mapping #:x-conv x-conv #:y-conv y-conv #:group group)
  (define aes (mapping-override mapping local-mapping))
  (define discrete-color (hash-ref aes 'discrete-color #f))
  (define facet-on (hash-ref aes 'facet #f))
  (define alpha (hash-ref aes 'alpha 1))
  
  (define tbl (make-hash))
  (for ([(x y strat facet)
         (in-data-frame* data (hash-ref aes 'x) (hash-ref aes 'y) discrete-color facet-on)]
        #:when (and x y)
        #:when (if group (equal? facet group) #t))
    (hash-update! tbl strat (cons (vector (x-conv x) (y-conv y)) _) null))
  
  (let ([color-n -1])
    (hash-map tbl
              (λ (strat pts)
                (set! color-n (add1 color-n))
                (points pts #:color (->pen-color color-n) #:label strat #:alpha alpha))
              #t)))

#lang racket
(require fancy-app plot/pict plot/utils "util.rkt")
(provide pdensity)

(define ((pdensity #:mapping [local-mapping (make-hash)])
         #:data data #:gmapping mapping #:x-conv x-conv #:y-conv y-conv #:group group)
  (define aes (mapping-override mapping local-mapping))
  (define alpha (hash-ref aes 'alpha 1))

  (define tbl (make-hash))
  (for ([(x strat facet)
         (in-data-frame* data (hash-ref aes 'x) (hash-ref aes 'discrete-color #f)
                         (hash-ref aes 'facet #f))]
        #:when x
        #:when (if group (equal? facet group) #t))
    (hash-update! tbl strat (cons (x-conv x) _) null))

  (let ([color-n -1])
    (hash-map tbl
              (λ (strat pts)
                (set! color-n (add1 color-n))
                (density pts #:color (->pen-color color-n) #:label strat #:alpha alpha))
              #t)))

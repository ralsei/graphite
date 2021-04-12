#lang racket
(require fancy-app plot/pict plot/utils "util.rkt")
(provide pdensity)

(define ((pdensity #:mapping [local-mapping (make-hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define alpha (hash-ref aes 'alpha 1))

  (define tbl (make-hash))
  (for ([(x strat facet)
         (in-data-frame* (gr-data) (hash-ref aes 'x) (hash-ref aes 'discrete-color #f)
                         (hash-ref aes 'facet #f))]
        #:when x
        #:when (equal? facet (gr-group)))
    (hash-update! tbl strat (cons ((gr-x-conv) x) _) null))

  (let ([color-n -1])
    (hash-map tbl
              (λ (strat pts)
                (set! color-n (add1 color-n))
                (run-renderer #:renderer density #:mapping aes
                              #:color (->pen-color color-n) #:label strat #:alpha alpha
                              pts))
              #t)))

#lang racket
(require fancy-app plot/utils
         (prefix-in plot: plot/pict)
         "util.rkt")
(provide lines)

(define ((lines #:mapping [local-mapping (make-hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))

  (define tbl (make-hash))
  (for ([(x y strat facet)
         (in-data-frame* (gr-data) (hash-ref aes 'x) (hash-ref aes 'y)
                         (hash-ref aes 'discrete-color #f) (hash-ref aes 'facet #f))]
        #:when (and x y)
        #:when (equal? facet (gr-group)))
    (hash-update! tbl strat (cons (vector ((gr-x-conv) x) ((gr-y-conv) y)) _) null))

  (let ([color-n -1])
    (hash-map tbl
              (λ (strat pts)
                (set! color-n (add1 color-n))
                (run-renderer #:renderer plot:lines #:mapping aes
                              #:color (->pen-color color-n) #:label strat
                              pts))
              #t)))

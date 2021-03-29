#lang racket
(require fancy-app plot/pict plot/utils
         "util.rkt")
(provide ppoints)

(define ((ppoints #:mapping [local-mapping (make-hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define discrete-color (hash-ref aes 'discrete-color #f))
  (define facet-on (hash-ref aes 'facet #f))
  
  (define tbl (make-hash))
  (for ([(x y strat facet)
         (in-data-frame* (gr-data) (hash-ref aes 'x) (hash-ref aes 'y) discrete-color facet-on)]
        #:when (and x y)
        #:when (equal? facet (gr-group)))
    (hash-update! tbl strat (cons (vector ((gr-x-conv) x) ((gr-y-conv) y)) _) null))
  
  (let ([color-n -1])
    (hash-map tbl
              (λ (strat pts)
                (set! color-n (add1 color-n))
                (run-renderer points aes pts #:color (->pen-color color-n) #:label strat))
                ;(points pts #:color (->pen-color color-n) #:label strat #:alpha alpha))
              #t)))

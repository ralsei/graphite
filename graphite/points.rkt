#lang racket
(require fancy-app pict plot/utils
         (prefix-in plot: plot/pict)
         "contracts.rkt"
         "util.rkt")
(provide
 (contract-out [points (->* ()
                            (#:mapping (aes-containing/c #:x string?
                                                         #:y string?
                                                         #:facet (or/c string? #f)
                                                         #:discrete-color (or/c string? #f)
                                                         #:x-min (or/c rational? #f)
                                                         #:x-max (or/c rational? #f)
                                                         #:y-min (or/c rational? #f)
                                                         #:y-max (or/c rational? #f)
                                                         #:sym point-sym/c
                                                         #:color plot-color/c
                                                         #:fill-color (or/c plot-color/c 'auto)
                                                         #:x-jitter (>=/c 0)
                                                         #:y-jitter (>=/c 0)
                                                         #:size (>=/c 0)
                                                         #:line-width (>=/c 0)
                                                         #:alpha (real-in 0 1)
                                                         #:label (or/c string? pict? #f)))
                            graphite-renderer?)]))

(define-renderer (points #:mapping [local-mapping (make-hash)]) ()
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
                (run-renderer #:renderer plot:points
                              #:mapping aes
                              #:color (->pen-color color-n) #:label strat
                              pts))
              #t)))

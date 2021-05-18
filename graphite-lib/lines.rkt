#lang racket
(require fancy-app pict plot/utils
         (prefix-in plot: plot/pict)
         "contracts.rkt" "util.rkt")
(provide
 (contract-out [lines (->* ()
                           (#:x-min (or/c rational? #f)
                            #:x-max (or/c rational? #f)
                            #:y-min (or/c rational? #f)
                            #:y-max (or/c rational? #f)
                            #:color plot-color/c
                            #:width (>=/c 0)
                            #:style plot-pen-style/c
                            #:alpha (real-in 0 1)
                            #:label (or/c string? pict? #f)
                            #:mapping (aes-containing/c #:x string?
                                                        #:y string?
                                                        #:facet (or/c string? #f)
                                                        #:discrete-color (or/c string? #f)))
                           graphite-renderer/c)]))

(define-renderer (lines #:kws kws #:kw-args kw-args
                        #:mapping [local-mapping (make-hash)]) ()
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
                (run-renderer #:renderer plot:lines #:kws kws #:kw-args kw-args
                              #:color (->pen-color color-n) #:label strat
                              pts))
              #t)))

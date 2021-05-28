#lang racket/base
(require fancy-app
         pict
         (prefix-in plot: plot/no-gui)
         plot/utils
         racket/contract/base
         "aes.rkt"
         "renderer.rkt"
         "util.rkt")

(provide
 (contract-out [density (->* ()
                             (#:x-min (or/c rational? #f)
                              #:x-max (or/c rational? #f)
                              #:y-min (or/c rational? #f)
                              #:y-max (or/c rational? #f)
                              #:samples (and/c exact-integer? (>=/c 2))
                              #:color plot-color/c
                              #:width (>=/c 0)
                              #:style plot-pen-style/c
                              #:alpha (real-in 0 1)
                              #:label (or/c string? pict? #f)
                              #:mapping (aes-containing/c #:x string?
                                                          #:discrete-color (or/c string? #f)
                                                          #:facet (or/c string? #f)))
                             graphite-renderer?)]))

(define-renderer (density #:kws kws #:kw-args kw-args
                          #:mapping [local-mapping (aes)])
                 (#:y-label "density")
  (define aes (mapping-override (gr-global-mapping) local-mapping))

  (define tbl (make-hash))
  (for ([(x strat facet)
         (in-data-frame* (gr-data) (hash-ref aes 'x) (hash-ref aes 'discrete-color #f)
                         (hash-ref aes 'facet #f))]
        #:when x
        #:when (equal? facet (gr-group)))
    (hash-update! tbl strat (cons ((gr-x-conv) x) _) null))

  (for/list ([(strat pts) (in-hash/sort tbl)]
             [color-n (in-naturals)])
    (run-renderer #:renderer plot:density #:kws kws #:kw-args kw-args
                  #:color (->pen-color color-n) #:label strat
                  pts)))

#lang racket/base
(require data/ddict
         fancy-app
         pict
         (prefix-in plot: plot/no-gui)
         plot/utils
         racket/contract/base
         "aes.rkt"
         "renderer.rkt"
         "util.rkt")

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
                           graphite-renderer?)]))

(define-renderer (lines #:kws kws #:kw-args kw-args
                        #:mapping [local-mapping (aes)]) ()
  (define aes (mapping-override (gr-global-mapping) local-mapping))

  (define tbl (make-mutable-ddict))
  (for ([(x y strat facet)
         (in-data-frame* (gr-data) (hash-ref aes 'x) (hash-ref aes 'y)
                         (hash-ref aes 'discrete-color #f) (hash-ref aes 'facet #f))]
        #:when (and x y)
        #:when (equal? facet (gr-group)))
    (ddict-update! tbl strat (cons (vector ((gr-x-conv) x) ((gr-y-conv) y)) _) null))

  (for/list ([(strat pts) (in-ddict tbl)]
             [color-n (in-naturals)])
    (run-renderer #:renderer plot:lines #:kws kws #:kw-args kw-args
                  #:color (->pen-color color-n) #:label strat
                  pts)))

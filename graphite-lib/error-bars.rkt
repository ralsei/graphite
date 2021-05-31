#lang racket/base
(require (prefix-in plot: plot/no-gui)
         plot/utils
         racket/contract/base
         "aes.rkt"
         "renderer.rkt"
         "util.rkt")

(provide
 (contract-out [error-bars (->* (#:mapping (and/c (aes-with/c #:perc-error string?)
                                                  (aes-containing/c #:x string?
                                                                    #:y string?
                                                                    #:facet (or/c string? #f))))
                                (#:x-min (or/c rational? #f)
                                 #:x-max (or/c rational? #f)
                                 #:y-min (or/c rational? #f)
                                 #:y-max (or/c rational? #f)
                                 #:color plot-color/c
                                 #:line-width (>=/c 0)
                                 #:line-style plot-pen-style/c
                                 #:width (>=/c 0)
                                 #:alpha (real-in 0 1)
                                 #:invert? boolean?)
                                graphite-renderer?)]))

(define-renderer (error-bars #:kws kws #:kw-args kw-args
                             #:mapping local-mapping) ()
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define bars
    (for/list ([(x y δ facet) (in-data-frame* (gr-data)
                                              (hash-ref aes 'x)
                                              (hash-ref aes 'y)
                                              (hash-ref aes 'perc-error)
                                              (hash-ref aes 'facet #f))]
               #:when (and x y δ)
               #:when (equal? facet (gr-group)))
      (vector x y (* y δ))))
  (run-renderer #:renderer plot:error-bars
                #:kws kws #:kw-args kw-args
                bars))

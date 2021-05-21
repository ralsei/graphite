#lang racket
(require fancy-app pict plot/utils
         (prefix-in plot: plot/pict)
         "aes.rkt" "renderer.rkt" "util.rkt")
(provide
 (contract-out [points (->* ()
                            (#:x-min (or/c rational? #f)
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
                             #:label (or/c string? pict? #f)
                             #:mapping (aes-containing/c #:x string?
                                                         #:y string?
                                                         #:facet (or/c string? #f)
                                                         #:discrete-color (or/c string? #f)
                                                         #:continuous-color (or/c string? #f)))
                            graphite-renderer?)]))

(define-renderer (points #:kws kws #:kw-args kw-args
                         #:mapping [local-mapping (aes)]) ()
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define discrete-color (hash-ref aes 'discrete-color #f))
  (define continuous-color (hash-ref aes 'continuous-color #f))
  (define facet-on (hash-ref aes 'facet #f))

  (when (and discrete-color continuous-color)
    (error 'points "cannot have both discrete and continuous color aesthetics"))

  (define tbl (make-hash))
  (for ([(x y strat facet)
         (in-data-frame* (gr-data) (hash-ref aes 'x) (hash-ref aes 'y)
                         (or discrete-color continuous-color) facet-on)]
        #:when (and x y)
        #:when (equal? facet (gr-group)))
    (hash-update! tbl
                  strat
                  (cons (vector ((gr-x-conv) x) ((gr-y-conv) y)) _) null))

  (define continuous-min (and continuous-color
                              (apply min (hash-keys tbl))))
  (define continuous-max (and continuous-color
                              (apply max (hash-keys tbl))))

  (let ([color-n -1])
    (hash-map tbl
              (λ (strat pts)
                (set! color-n (add1 color-n))
                (run-renderer #:renderer plot:points
                              #:kws kws #:kw-args kw-args
                              #:color (if continuous-color
                                          (->pen-color
                                           (inexact->exact
                                            (round (convert continuous-min 0
                                                            continuous-max
                                                            (color-map-size (plot-pen-color-map))
                                                            strat))))
                                          (->pen-color color-n))
                              #:label (and discrete-color strat)
                              pts))
              #t)))

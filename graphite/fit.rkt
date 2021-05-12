#lang racket
(require pict plot/pict plot/utils
         simple-polynomial/base simple-polynomial/fit
         "contracts.rkt"
         "util.rkt")
(provide
 (contract-out [fit (->* ()
                         (#:degree positive-integer?
                          #:show-equation? boolean?
                          #:mapping (aes-containing/c #:x string?
                                                      #:y string?
                                                      #:facet (or/c string? #f)
                                                      #:y-min (or/c rational? #f)
                                                      #:y-max (or/c rational? #f)
                                                      #:samples (and/c exact-integer? (>=/c 2))
                                                      #:color plot-color/c
                                                      #:width (>=/c 0)
                                                      #:style plot-pen-style/c
                                                      #:alpha (real-in 0 1)
                                                      #:label (or/c string? pict? #f)))
                         graphite-renderer?)]))

(define ((fit #:degree [degree 1] #:show-equation? [show-equation? #f]
              #:mapping [local-mapping (hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define fit-line
    (for/fold ([pts '()]
               #:result (points->best-fit-polynomial pts degree))
              ([(x y facet) (in-data-frame* (gr-data) (hash-ref aes 'x)
                                            (hash-ref aes 'y)
                                            (hash-ref aes 'facet #f))]
               #:when (and x y)
               #:when (equal? facet (gr-group)))
      (cons (list ((gr-x-conv) x) ((gr-y-conv) y)) pts)))
  (run-renderer #:renderer function
                #:mapping (if show-equation?
                              (hash-set aes 'label (poly->string fit-line))
                              aes)
                fit-line))

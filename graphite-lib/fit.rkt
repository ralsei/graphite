#lang racket/base
(require loess
         pict
         plot/no-gui
         plot/utils
         racket/contract/base
         racket/math
         racket/match
         simple-polynomial/base
         simple-polynomial/fit
         "aes.rkt"
         "renderer.rkt"
         "util.rkt")

(provide
 (contract-out [fit (->* ()
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
                          #:method (or/c 'poly 'loess)
                          #:span (real-in 0 1)
                          #:degree positive-integer?
                          #:show-equation? boolean?
                          #:mapping (aes-containing/c #:x string?
                                                      #:y string?
                                                      #:facet (or/c string? #f)))
                         graphite-renderer?)]))

(define-renderer (fit #:kws kws #:kw-args kw-args
                      #:method [method 'poly]
                      #:x-min [x-min #f] #:x-max [x-max #f]
                      #:span [span 0.75] #:degree [degree 1]
                      #:show-equation? [show-equation? #f]
                      #:mapping [local-mapping (aes)]) ()
  (when (and (eq? method 'loess) show-equation?)
    (error 'fit "loess fit cannot be expressed as an equation"))

  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define pts
    (for/list ([(x y facet) (in-data-frame* (gr-data) (hash-ref aes 'x)
                                            (hash-ref aes 'y)
                                            (hash-ref aes 'facet #f))]
               #:when (and x y)
               #:when (equal? facet (gr-group)))
      (list ((gr-x-conv) x) ((gr-y-conv) y))))
  (define fit-line
    (match method
      ['poly (points->best-fit-polynomial pts degree)]
      ['loess (loess-fit (apply vector (map car pts)) (apply vector (map cadr pts))
                         #:span span #:degree degree)]))
  (run-renderer #:renderer function
                #:kws kws #:kw-args kw-args
                #:label (if show-equation? (poly->string fit-line) #f)
                fit-line x-min x-max))

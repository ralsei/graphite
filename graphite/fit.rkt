#lang racket
(require bestfit pict plot/pict plot/utils
         "contracts.rkt"
         "util.rkt")
(provide
 (contract-out [fit (->* ()
                         (#:method (or/c 'linear 'log 'exp 'power)
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

(define ((fit #:method [method 'linear] #:mapping [local-mapping (make-hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define facet-on (hash-ref aes 'facet #f))
  (define fit-function
    (match method
       ['linear linear-fit]
       ['exp exp-fit]
       ['power power-fit]
       ['log log-fit]))
  (define fit-line
    (for/lists (xs ys #:result (fit-function xs ys))
               ([(x y facet) (in-data-frame* (gr-data) (hash-ref aes 'x) (hash-ref aes 'y) facet-on)]
                #:when (and x y)
                #:when (equal? facet (gr-group)))
      (values (exact->inexact ((gr-x-conv) x)) (exact->inexact ((gr-y-conv) y)))))
  (run-renderer #:renderer function
                #:mapping aes
                fit-line))

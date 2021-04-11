#lang racket
(require plot/pict "util.rkt")

; XXX: should we support multiple facets? n facets?
(define (facet-plot render-fns)
  (define facet (hash-ref (gr-global-mapping) 'facet))
  (define groups (vector-sort (possibilities (gr-data) facet) string-ci<?))

  (define initial-plot
    (apply (curry pplot
                  #:data (gr-data) #:mapping (gr-global-mapping)
                  #:x-conv (gr-x-conv) #:y-conv (gr-y-conv)
                  #:group (vector-ref groups 0) #:title (~a (vector-ref groups 0)))
           render-fns))

  (for/fold ([plt initial-plot])
            ([grp (vector-drop groups 1)])
    ; all other arguments should be handled by the initial parameterize call
    (hc-append plt
               (apply (curry pplot
                             #:data (gr-data) #:mapping (gr-global-mapping)
                             #:x-conv (gr-x-conv) #:y-conv (gr-y-conv)
                             #:group grp #:title (~a grp)
                             #:y-ticks no-ticks #:y-label #f)
                      render-fns))))

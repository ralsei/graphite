#lang racket
(require file/convertible pict plot/pict fancy-app
         "bar.rkt"
         "density.rkt"
         "histogram.rkt"
         "fit.rkt"
         "lines.rkt"
         "points.rkt"
         "util.rkt")
(provide pplot aes save-pict
         (all-from-out "bar.rkt")
         (all-from-out "density.rkt")
         (all-from-out "histogram.rkt")
         (all-from-out "fit.rkt")
         (all-from-out "lines.rkt")
         (all-from-out "points.rkt"))

(define aes
  (make-keyword-procedure
   (λ (kws kw-args . rst)
     (when (not (empty? rst))
       (error 'aes "called with non-keyword argument"))
     (for/hash ([kw (in-list kws)]
                [kwa (in-list kw-args)])
       (values (keyword->symbol kw) kwa)))))

; XXX: should we support multiple facets? n facets?
(define (facet-plot render-fns)
  (define facet (hash-ref (gr-global-mapping) 'facet))
  (define groups (vector-sort (possibilities (gr-data) facet) string-ci<?))

  (define (all-plots [y-min #f] [y-max #f])
    (for/list ([grp (in-vector groups)])
      (apply (curry pplot
                    #:data (gr-data) #:mapping (gr-global-mapping)
                    #:x-conv (gr-x-conv) #:y-conv (gr-y-conv)
                    #:group grp #:title (~a grp)
                    #:y-min y-min #:y-max y-max)
             render-fns)))

  (define all-y-bounds (map (compose (vector-ref _ 1) plot-pict-bounds) (all-plots)))
  (define y-min (apply min (map (vector-ref _ 0) all-y-bounds)))
  (define y-max (apply max (map (vector-ref _ 1) all-y-bounds)))

  (define initial-plot
    (apply (curry pplot
                  #:data (gr-data) #:mapping (gr-global-mapping)
                  #:x-conv (gr-x-conv) #:y-conv (gr-y-conv)
                  #:group (vector-ref groups 0) #:title (~a (vector-ref groups 0))
                  #:y-min y-min #:y-max y-max)
           render-fns))

  (for/fold ([plt initial-plot])
            ([app
              (parameterize ([plot-y-ticks no-ticks]
                             [plot-y-label #f])
                (rest (all-plots y-min y-max)))])
    ; all other arguments should be handled by the initial parameterize call
    (hc-append plt app)))

(define (get-conversion-function conv transform)
  (cond [(and conv transform) (compose (invertible-function-f transform) conv)]
        [conv conv]
        [transform (invertible-function-f transform)]
        [else identity]))

(define (pplot #:data data #:mapping mapping
               #:width [width (plot-width)]
               #:height [height (plot-height)]
               #:title [title (plot-title)]
               #:x-label [x-label (plot-x-label)]
               #:x-transform [x-transform #f]
               #:x-ticks [x-ticks (plot-x-ticks)]
               #:x-conv [x-conv (gr-x-conv)]
               #:x-min [x-min (gr-x-min)]
               #:x-max [x-max (gr-x-max)]
               #:y-label [y-label (plot-y-label)]
               #:y-transform [y-transform #f]
               #:y-ticks [y-ticks (plot-y-ticks)]
               #:y-conv [y-conv (gr-y-conv)]
               #:y-min [y-min (gr-y-min)]
               #:y-max [y-max (gr-y-max)]
               #:legend-anchor [legend-anchor (plot-legend-anchor)]
               #:group [group (gr-group)]
               . render-fns)
  (parameterize ([plot-title title]
                 [plot-width width]
                 [plot-height height]
                 [plot-x-label x-label]
                 [plot-y-label y-label]
                 [plot-x-ticks
                  (if x-transform
                      (ticks-scale x-ticks (invertible-inverse x-transform))
                      x-ticks)]
                 [plot-y-ticks
                  (if y-transform
                      (ticks-scale y-ticks (invertible-inverse y-transform))
                      y-ticks)]
                 [plot-legend-anchor legend-anchor]
                 ; better defaults
                 [plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-font-face "Arial"]
                 [point-sym 'bullet]
                 [plot-pen-color-map (hash-ref mapping 'colormap 'set1)]
                 ; our settings
                 [gr-data data]
                 [gr-global-mapping mapping]
                 [gr-x-conv (get-conversion-function x-conv x-transform)]
                 [gr-y-conv (get-conversion-function y-conv y-transform)]
                 [gr-group group]
                 [gr-x-min x-min]
                 [gr-x-max x-max]
                 [gr-y-min y-min]
                 [gr-y-max y-max])
    (define facet (hash-ref mapping 'facet #f))
    (cond [(and (not group) facet) (facet-plot render-fns)]
          [else (plot-pict #:x-min x-min
                           #:x-max x-max
                           #:y-min y-min
                           #:y-max y-max
                           (for/list ([render-fn (in-list render-fns)])
                             (render-fn)))])))

(define (save-pict pict path)
  (define ext (path-get-extension path))
  (match ext
    [(or #".png" #".pdf" #".svg")
     (with-output-to-file path
       (lambda () (write-bytes (convert pict
                                        (string->symbol
                                         (string-append (bytes->string/utf-8 (subbytes ext 1))
                                                        "-bytes"))))))]
    [_ (error 'save-pict "unsupported extension")]))

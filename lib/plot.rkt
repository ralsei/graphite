#lang racket
(require file/convertible pict plot/pict
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
  (for/fold ([plt (blank)])
            ([grp (possibilities (gr-data) facet)])
    ; all other arguments should be handled by the initial parameterize call
    (hc-append plt
               (apply (curry pplot
                             #:data (gr-data) #:mapping (gr-global-mapping)
                             #:x-conv (gr-x-conv) #:y-conv (gr-y-conv)
                             #:group grp #:title (~a grp))
                      render-fns))))

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
               #:x-conv [x-conv #f]
               #:x-max [x-max #f]
               #:y-label [y-label (plot-y-label)]
               #:y-transform [y-transform #f]
               #:y-ticks [y-ticks (plot-y-ticks)]
               #:y-conv [y-conv #f]
               #:y-max [y-max #f]
               #:group [group #f]
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
                 [gr-group group])
    (define facet (hash-ref mapping 'facet #f))
    (cond [(and (not group) facet) (facet-plot render-fns)]
          [else (plot #:x-max x-max #:y-max y-max
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

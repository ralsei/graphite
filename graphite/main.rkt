#lang racket
(require file/convertible racket/dict pict fancy-app
         (except-in plot/pict density lines points)
         "bar.rkt"
         "boxplot.rkt"
         "density.rkt"
         "histogram.rkt"
         "fit.rkt"
         "lines.rkt"
         "points.rkt"
         "util.rkt")
(provide graph aes save-pict
         no-transform logarithmic-transform
         (all-from-out "bar.rkt")
         (all-from-out "boxplot.rkt")
         (all-from-out "density.rkt")
         (all-from-out "histogram.rkt")
         (all-from-out "fit.rkt")
         (all-from-out "lines.rkt")
         (all-from-out "points.rkt"))

(define no-transform (invertible-function identity identity))
(define logarithmic-transform (invertible-function (log _ 10) (expt 10 _)))

(define aes
  (make-keyword-procedure
   (λ (kws kw-args . rst)
     (when (not (empty? rst))
       (error 'aes "called with non-keyword argument"))
     (for/hash ([kw (in-list kws)]
                [kwa (in-list kw-args)])
       (values (keyword->symbol kw) kwa)))))

(define (graph-list render-fns grp)
  (keyword-apply/dict
   (curry graph
          #:data (gr-data)
          #:mapping (if grp (gr-global-mapping) (hash-remove (gr-global-mapping) 'facet))
          #:x-conv (gr-x-conv) #:y-conv (gr-y-conv))
   (if grp
       (hash '#:group grp '#:title (~a grp))
       (hash))
   render-fns))

; XXX: should we support multiple facets? n facets?
(define (facet-plot render-fns)
  (define facet (hash-ref (gr-global-mapping) 'facet))
  (define groups (vector-sort (possibilities (gr-data) facet)
                              (λ (x y) (string-ci<? (~a x) (~a y)))))

  (define init-plot (graph-list render-fns #f))
  (match-define (vector (vector x-min x-max)
                        (vector y-min y-max))
    (plot-pict-bounds init-plot))
  (match-define (vector x-left y-bottom) ((plot-pict-plot->dc init-plot) (vector x-min y-min)))
  (match-define (vector x-right y-top) ((plot-pict-plot->dc init-plot) (vector x-max y-max)))
  (define aspect-ratio
    (abs (/ (- x-right x-left)
            (- y-top y-bottom))))

  (parameterize ([gr-x-min (if (not (gr-x-min)) x-min (gr-x-min))]
                 [gr-x-max (if (not (gr-x-max)) x-max (gr-x-max))]
                 [gr-y-min (if (not (gr-y-min)) y-min (gr-y-min))]
                 [gr-y-max (if (not (gr-y-max)) y-max (gr-y-max))]
                 [plot-aspect-ratio aspect-ratio])
    (for/fold ([plt (graph-list render-fns (vector-ref groups 0))])
              ([grp (vector-drop groups 1)])
      ; all other arguments should be handled by the initial parameterize call
      (parameterize ([plot-y-ticks no-ticks]
                     [plot-y-label #f]
                     [plot-width (inexact->exact (- x-right x-left))])
        (hc-append plt (graph-list render-fns grp))))))

(define (get-conversion-function conv transform)
  (cond [(and conv transform) (compose (invertible-function-f transform) conv)]
        [conv conv]
        [transform (invertible-function-f transform)]
        [else identity]))

(define (graph #:data data #:mapping mapping
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

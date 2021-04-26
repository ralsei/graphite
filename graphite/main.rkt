#lang racket
(require file/convertible data-frame pict fancy-app kw-utils/kw-hash-lambda
         plot/utils
         (except-in plot/pict density lines points)

         "contracts.rkt"
         "transforms.rkt"
         "util.rkt"

         "bar.rkt"
         "boxplot.rkt"
         "density.rkt"
         "histogram.rkt"
         "fit.rkt"
         "lines.rkt"
         "points.rkt")
(provide
 (contract-out [graph (->* (#:data data-frame? #:mapping aes?)
                           (#:width (or/c rational? #f)
                            #:height (or/c rational? #f)
                            #:title (or/c string? pict? #f)
                            #:x-label (or/c string? pict? #f)
                            #:x-transform (or/c transform? #f)
                            #:x-conv (or/c (-> any/c real?) #f)
                            #:x-min (or/c rational? #f)
                            #:x-max (or/c rational? #f)
                            #:y-label (or/c string? pict? #f)
                            #:y-transform (or/c transform? #f)
                            #:y-conv (or/c (-> any/c real?) #f)
                            #:y-min (or/c rational? #f)
                            #:y-max (or/c rational? #f)
                            #:legend-anchor legend-anchor/c)
                           #:rest (non-empty-listof graphite-renderer?)
                           pict?)])
 aes save-pict
 (all-from-out "bar.rkt")
 (all-from-out "boxplot.rkt")
 (all-from-out "contracts.rkt")
 (all-from-out "density.rkt")
 (all-from-out "histogram.rkt")
 (all-from-out "fit.rkt")
 (all-from-out "lines.rkt")
 (all-from-out "points.rkt")
 (all-from-out "transforms.rkt"))

(define aes
  (kw-hash-lambda args #:kws kw-hash
    (when (not (empty? args))
      (error 'aes "called with non-keyword argument"))
    (for/hash ([(k v) (in-hash kw-hash)])
      (values (keyword->symbol k) v))))

; XXX: should we support multiple facets? n facets?
(define (facet-plot render-fns)
  (define facet (hash-ref (gr-global-mapping) 'facet))
  (define groups (vector-sort (possibilities (gr-data) facet)
                              (λ (x y) (string-ci<? (~a x) (~a y)))))

  (define init-plot
    (parameterize ([gr-global-mapping (hash-remove (gr-global-mapping) 'facet)])
      (graph-internal #f render-fns)))
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
    (for/fold ([plt (graph-internal (vector-ref groups 0) render-fns)])
              ([grp (vector-drop groups 1)])
      ; all other arguments should be handled by the initial parameterize call
      (parameterize ([plot-y-ticks no-ticks]
                     [plot-y-label #f]
                     [plot-width (inexact->exact (- x-right x-left))])
        (hc-append plt (graph-internal grp render-fns))))))

(define (get-conversion-function conv transform)
  (cond [(and conv transform) (compose (invertible-function-f (transform-function transform)) conv)]
        [conv conv]
        [transform (invertible-function-f (transform-function transform))]
        [else identity]))

(define (graph-internal group render-fns)
  (define facet (and (hash-ref (gr-global-mapping) 'facet #f) #t))
  (cond [(and (not group) facet) (facet-plot render-fns)]
        [else
         (plot-pict #:x-min (gr-x-min)
                    #:x-max (gr-x-max)
                    #:y-min (gr-y-min)
                    #:y-max (gr-y-max)
                    #:title (or group (plot-title))
                    (parameterize ([gr-group group])
                      (for/list ([render-fn (in-list render-fns)])
                        (render-fn))))]))

(define (graph #:data data #:mapping mapping
               #:width [width (plot-width)]
               #:height [height (plot-height)]
               #:title [title (plot-title)]
               #:x-label [x-label (plot-x-label)]
               #:x-transform [x-transform #f]
               #:x-conv [x-conv (gr-x-conv)]
               #:x-min [x-min (gr-x-min)]
               #:x-max [x-max (gr-x-max)]
               #:y-label [y-label (plot-y-label)]
               #:y-transform [y-transform #f]
               #:y-conv [y-conv (gr-y-conv)]
               #:y-min [y-min (gr-y-min)]
               #:y-max [y-max (gr-y-max)]
               #:legend-anchor [legend-anchor (plot-legend-anchor)]
               . render-fns)
  (parameterize ([plot-title title]
                 [plot-width width]
                 [plot-height height]
                 [plot-x-label x-label]
                 [plot-y-label y-label]
                 [plot-x-ticks
                  (if x-transform
                      (get-adjusted-ticks x-transform)
                      (plot-x-ticks))]
                 [plot-y-ticks
                  (if y-transform
                      (ticks-scale y-ticks (invertible-inverse y-transform))
                      (plot-y-ticks))]
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
                 [gr-x-min x-min]
                 [gr-x-max x-max]
                 [gr-y-min y-min]
                 [gr-y-max y-max])
    (graph-internal #f render-fns)))

(define (save-pict pict path)
  (define ext (path-get-extension path))
  (match ext
    [(or #".png" #".pdf" #".svg")
     (with-output-to-file path
       (λ () (write-bytes (convert pict
                                   (string->symbol
                                    (string-append (bytes->string/utf-8 (subbytes ext 1))
                                                   "-bytes"))))))]
    [_ (error 'save-pict "unsupported extension")]))

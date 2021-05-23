#lang racket
(require file/convertible data-frame pict fancy-app
         plot/utils racket/hash
         (except-in plot/pict density lines points)

         "aes.rkt"
         "renderer.rkt"
         "transforms.rkt"
         (except-in "util.rkt" convert)

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
                            #:x-conv (or/c (-> any/c any/c) #f)
                            #:x-min (or/c rational? #f)
                            #:x-max (or/c rational? #f)
                            #:y-label (or/c string? pict? #f)
                            #:y-transform (or/c transform? #f)
                            #:y-conv (or/c (-> any/c any/c) #f)
                            #:y-min (or/c rational? #f)
                            #:y-max (or/c rational? #f)
                            #:facet-wrap (or/c positive-integer? #f)
                            #:legend-anchor legend-anchor/c)
                           #:rest (non-empty-listof graphite-renderer?)
                           pict?)])
 save-pict
 (struct-out graphite-renderer)

 (all-from-out "aes.rkt")
 (all-from-out "bar.rkt")
 (all-from-out "boxplot.rkt")
 (all-from-out "density.rkt")
 (all-from-out "histogram.rkt")
 (all-from-out "fit.rkt")
 (all-from-out "lines.rkt")
 (all-from-out "points.rkt")
 (all-from-out "transforms.rkt"))

; XXX: should we support multiple facets? n facets?
(define (facet-plot render-fns wrap)
  (define facet (hash-ref (gr-global-mapping) 'facet))
  (define groups (vector-sort (possibilities (gr-data) facet)
                              (λ (x y) (string-ci<? (~a x) (~a y)))))
  (define facet-wrap (or wrap (inexact->exact (ceiling (sqrt (vector-length groups))))))
  (define wrapped-groups (vector-reshape groups facet-wrap))

  (define metrics-plot
    (parameterize ([gr-global-mapping (hash-remove (gr-global-mapping) 'facet)]
                   [plot-title #f])
      (graph-internal #f render-fns wrap)))
  (match-define (vector (vector x-min x-max)
                        (vector y-min y-max))
    (plot-pict-bounds metrics-plot))
  (define-values (x-extras y-extras) (plot-extras-size metrics-plot))

  (define init-plot
    (parameterize ([gr-global-mapping (hash-remove (gr-global-mapping) 'facet)])
      (graph-internal #f render-fns wrap)))

  (define exact-x-extras (inexact->exact (round x-extras)))
  (define exact-y-extras (inexact->exact (round y-extras)))

  (define height-no-extras (round (/ (- (pict-height init-plot) exact-x-extras)
                                     (vector-length wrapped-groups))))
  (define width-no-extras (round (/ (- (pict-width init-plot) exact-y-extras)
                                    (vector-length (vector-ref wrapped-groups 0)))))

  (define (run-plot group [with-x-extras? #f] [with-y-extras? #f])
    (parameterize ([plot-x-ticks (if with-x-extras? (plot-x-ticks) no-ticks)]
                   [plot-x-label (if with-x-extras? (plot-x-label) #f)]
                   [plot-y-ticks (if with-y-extras? (plot-y-ticks) no-ticks)]
                   [plot-y-label (if with-y-extras? (plot-y-label) #f)]
                   [plot-height (if with-x-extras?
                                    (+ height-no-extras exact-x-extras)
                                    height-no-extras)]
                   [plot-width (if with-y-extras?
                                   (+ width-no-extras exact-y-extras)
                                   width-no-extras)])
      (if group
          (graph-internal group render-fns wrap)
          (filled-rectangle (plot-width) (plot-height)
                            #:color (plot-background)
                            #:border-color (plot-background)))))

  (define (plot-row group-vector [with-x-extras? #f])
    (for/fold ([plt (run-plot (vector-ref group-vector 0) with-x-extras? #t)])
              ([grp (vector-drop group-vector 1)])
      (hc-append plt (run-plot grp with-x-extras? #f))))

  (parameterize ([gr-x-min (if (not (gr-x-min)) x-min (gr-x-min))]
                 [gr-x-max (if (not (gr-x-max)) x-max (gr-x-max))]
                 [gr-y-min (if (not (gr-y-min)) y-min (gr-y-min))]
                 [gr-y-max (if (not (gr-y-max)) y-max (gr-y-max))])
    (vc-append
      (for/fold ([plt (blank)])
                ([grp-vector (in-vector (vector-drop-right wrapped-groups 1))])
        (vc-append plt (plot-row grp-vector)))
      (plot-row (vector-ref wrapped-groups (sub1 (vector-length wrapped-groups))) #t))))

(define (get-conversion-function conv transform)
  (cond [(and conv transform) (compose (transform-function transform) conv)]
        [conv conv]
        [transform (transform-function transform)]
        [else identity]))

(define (graph-internal group render-fns wrap)
  (define facet (and (hash-ref (gr-global-mapping) 'facet #f) #t))
  (cond [(and (not group) facet) (facet-plot render-fns wrap)]
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
               #:x-label [x-label #f]
               #:x-transform [x-transform #f]
               #:x-conv [x-conv (gr-x-conv)]
               #:x-min [x-min (gr-x-min)]
               #:x-max [x-max (gr-x-max)]
               #:y-label [y-label #f]
               #:y-transform [y-transform #f]
               #:y-conv [y-conv (gr-y-conv)]
               #:y-min [y-min (gr-y-min)]
               #:y-max [y-max (gr-y-max)]
               #:facet-wrap [facet-wrap #f]
               #:legend-anchor [legend-anchor (plot-legend-anchor)]
               . renderers)
  (define defaults
    (alist plot-x-label (hash-ref mapping 'x #f)
           plot-y-label (hash-ref mapping 'y #f)
           point-sym 'bullet
           plot-pen-color-map (or (plot-pen-color-map) 'set1)
           plot-x-far-ticks no-ticks
           plot-y-far-ticks no-ticks))
  (define user-data
    (alist plot-title title
           plot-width width
           plot-height height
           plot-x-label x-label
           plot-y-label y-label
           plot-x-ticks (and x-transform (get-adjusted-ticks x-transform))
           plot-y-ticks (and y-transform (get-adjusted-ticks y-transform))
           plot-legend-anchor legend-anchor))

  (define metadata (alist-remove-false
                    (append defaults
                            (append* (map graphite-renderer-metadata renderers))
                            user-data)))

  (parameterize ([gr-data data]
                 [gr-global-mapping mapping]
                 [gr-x-conv (get-conversion-function x-conv x-transform)]
                 [gr-y-conv (get-conversion-function y-conv y-transform)]
                 [gr-x-min x-min]
                 [gr-x-max x-max]
                 [gr-y-min y-min]
                 [gr-y-max y-max])
    (with-metadata metadata
      (graph-internal #f (map graphite-renderer-function renderers) facet-wrap))))

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

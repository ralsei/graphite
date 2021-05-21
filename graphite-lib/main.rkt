#lang racket
(require file/convertible data-frame pict fancy-app
         plot/utils racket/hash
         (except-in plot/pict density lines points)

         "contracts.rkt"
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
                            #:x-conv (or/c (-> any/c real?) #f)
                            #:x-min (or/c rational? #f)
                            #:x-max (or/c rational? #f)
                            #:y-label (or/c string? pict? #f)
                            #:y-transform (or/c transform? #f)
                            #:y-conv (or/c (-> any/c real?) #f)
                            #:y-min (or/c rational? #f)
                            #:y-max (or/c rational? #f)
                            #:legend-anchor legend-anchor/c)
                           #:rest (non-empty-listof graphite-renderer/c)
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

(define/kw (aes kws kw-args . rst)
  (when (not (empty? rst))
    (error 'aes "called with non-keyword argument"))
  (for/hash ([k (in-list kws)]
             [v (in-list kw-args)])
    (values (keyword->symbol k) v)))

; XXX: should we support multiple facets? n facets?
(define (facet-plot render-fns)
  (define facet (hash-ref (gr-global-mapping) 'facet))
  (define groups (vector-sort (possibilities (gr-data) facet)
                              (λ (x y) (string-ci<? (~a x) (~a y)))))
  (define facet-wrap (hash-ref (gr-global-mapping) 'facet-wrap
                               (inexact->exact (ceiling (sqrt (vector-length groups))))))
  (define wrapped-groups (vector-reshape groups facet-wrap))

  (define init-plot
    (parameterize ([gr-global-mapping (hash-remove (gr-global-mapping) 'facet)])
      (graph-internal #f render-fns)))
  (match-define (vector (vector x-min x-max)
                        (vector y-min y-max))
    (plot-pict-bounds init-plot))
  (match-define (vector x-left y-bottom) ((plot-pict-plot->dc init-plot) (vector x-min y-min)))
  (match-define (vector x-right y-top) ((plot-pict-plot->dc init-plot) (vector x-max y-max)))

  ; FIXME: this doesn't work w/ discrete-histogram ticks
  (define (plot-row group-vector [x-show-info? #f])
    (parameterize ([plot-x-ticks (if x-show-info? (plot-x-ticks) no-ticks)]
                   [plot-x-label (if x-show-info? (plot-x-label) #f)])
      (for/fold ([plt (graph-internal (vector-ref group-vector 0) render-fns)])
                ([grp (vector-drop group-vector 1)])
        (parameterize ([plot-y-ticks no-ticks]
                       [plot-y-label #f]
                       [plot-width (inexact->exact (- x-right x-left))])
          (hc-append plt
                     (if grp
                         (graph-internal grp render-fns)
                         (filled-rectangle (- x-right x-left)
                                           (pict-height init-plot)
                                           #:color (plot-background)
                                           #:border-color (plot-background))))))))

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
               #:x-label [x-label #f]
               #:x-transform [x-transform no-transform]
               #:x-conv [x-conv (gr-x-conv)]
               #:x-min [x-min (gr-x-min)]
               #:x-max [x-max (gr-x-max)]
               #:y-label [y-label #f]
               #:y-transform [y-transform no-transform]
               #:y-conv [y-conv (gr-y-conv)]
               #:y-min [y-min (gr-y-min)]
               #:y-max [y-max (gr-y-max)]
               #:legend-anchor [legend-anchor (plot-legend-anchor)]
               . renderers)
  ; TODO: better metadata conflict detection
  (define metadata (apply (curry hash-union #:combine (λ (x y) x))
                          (map renderer-metadata renderers)))

  (parameterize ([plot-title title]
                 [plot-width width]
                 [plot-height height]
                 [plot-x-label (if x-label x-label (hash-ref mapping 'x #f))]
                 [plot-y-label
                  (cond [(hash-ref metadata 'y-label #f) => identity]
                        [y-label y-label]
                        [else (hash-ref mapping 'y #f)])]
                 [plot-x-ticks
                  (let ([maybe-x-ticks (hash-ref metadata 'x-ticks #f)])
                    (if maybe-x-ticks
                        maybe-x-ticks
                        (get-adjusted-ticks x-transform)))]
                 [plot-y-ticks
                  (let ([maybe-y-ticks (hash-ref metadata 'y-ticks #f)])
                    (if maybe-y-ticks
                        maybe-y-ticks
                       (get-adjusted-ticks y-transform)))]
                 [plot-legend-anchor legend-anchor]
                 ; better defaults
                 [plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [point-sym 'bullet]
                 [plot-pen-color-map (or (plot-pen-color-map) 'set1)]
                 ; our settings
                 [gr-data data]
                 [gr-global-mapping mapping]
                 [gr-x-conv (get-conversion-function x-conv x-transform)]
                 [gr-y-conv (get-conversion-function y-conv y-transform)]
                 [gr-x-min x-min]
                 [gr-x-max x-max]
                 [gr-y-min y-min]
                 [gr-y-max y-max])
    (graph-internal #f (map renderer-function renderers))))

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

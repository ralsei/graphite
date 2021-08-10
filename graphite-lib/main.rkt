#lang racket/base
(require data-frame
         fancy-app
         file/convertible
         pict
         plot/utils
         racket/bool
         racket/contract/base
         racket/list
         racket/match
         racket/math
         racket/path
         "aes.rkt"
         "bar.rkt"
         "boxplot.rkt"
         "col.rkt"
         "density.rkt"
         "error-bars.rkt"
         "faceting.rkt"
         "fit.rkt"
         "histogram.rkt"
         "lines.rkt"
         "points.rkt"
         "qualitative.rkt"
         "renderer.rkt"
         "titles.rkt"
         "themes.rkt"
         "transforms.rkt"
         (except-in "util.rkt" convert)
         "with-area.rkt")

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
                            #:legend-anchor legend-anchor/c
                            #:theme graphite-theme?)
                           #:rest (non-empty-listof graphite-renderer?)
                           pict?)])
 save-pict
 (struct-out graphite-renderer)

 (all-from-out "aes.rkt")
 (all-from-out "bar.rkt")
 (all-from-out "boxplot.rkt")
 (all-from-out "col.rkt")
 (all-from-out "density.rkt")
 (all-from-out "error-bars.rkt")
 (all-from-out "histogram.rkt")
 (all-from-out "fit.rkt")
 (all-from-out "lines.rkt")
 (all-from-out "points.rkt")
 (all-from-out "themes.rkt")
 (all-from-out "transforms.rkt"))

;; ... all that stuff... -> pict?
;; runs everything
(define (graph #:data data #:mapping mapping
               #:width [width (plot-width)]
               #:height [height (plot-height)]
               #:title [ttitle (plot-title)]
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
               #:theme [theme theme-default]
               . renderers)
  ;; parameterize data, mappings, etc
  (parameterize ([gr-data data]
                 [gr-global-mapping mapping]
                 [gr-x-conv x-conv] ; temporary, in case axes are not reals
                 [gr-y-conv y-conv]
                 [gr-x-min x-min]
                 [gr-x-max x-max]
                 [gr-y-min y-min]
                 [gr-y-max y-max])
    (define render-fns (map graphite-renderer-function renderers))

    ; calculate bounds, for axis transforms
    ; don't bother with titles, since we add our own later
    (define metric-plot
      (parameterize ([plot-pen-color-map 'set1]
                     [plot-brush-color-map 'set1]
                     [plot-width width]
                     [plot-height height]
                     [plot-title #f] [plot-x-label #f] [plot-y-label #f]
                     [gr-global-mapping (hash-remove (gr-global-mapping) 'facet)])
        (graph-internal render-fns)))
    (match-define (vector (vector actual/x-min actual/x-max)
                          (vector actual/y-min actual/y-max))
      (plot-pict-bounds metric-plot))

    ; determine what axes are qualitative. if they are, don't show ticks
    (define x-qualitative? (and (hash-has-key? mapping 'x)
                                (qualitative? mapping 'x)))
    (define y-qualitative? (and (hash-has-key? mapping 'y)
                                (qualitative? mapping 'y)))

    ; overridden by anything
    (define defaults
      (alist gr-x-label (hash-ref mapping 'x #f)
             gr-y-label (hash-ref mapping 'y #f)
             point-sym 'bullet
             plot-x-ticks (and x-qualitative? no-ticks)
             plot-y-ticks (and y-qualitative? no-ticks)
             plot-x-far-ticks no-ticks
             plot-y-far-ticks no-ticks))
    ; overrides anything
    (define user-data
      (alist plot-width width
             plot-height height
             gr-title ttitle
             gr-x-label x-label
             gr-y-label y-label
             plot-x-ticks (and x-transform (get-adjusted-ticks actual/x-min actual/x-max x-transform))
             plot-y-ticks (and y-transform (get-adjusted-ticks actual/y-min actual/y-max y-transform))
             plot-legend-anchor legend-anchor))

    ; an alist of parameters
    (define metadata (alist-remove-false
                      (append defaults
                              (append* (map graphite-renderer-metadata renderers))
                              (theme->alist theme)
                              user-data)))

    (parameterize ([gr-x-conv (get-conversion-function actual/x-min actual/x-max x-conv x-transform)]
                   [gr-y-conv (get-conversion-function actual/y-min actual/y-max y-conv y-transform)]
                   [plot-title #f]
                   [plot-x-label #f]
                   [plot-y-label #f])
      (with-metadata metadata
        (define fs (inexact->exact (round (pict-height (title "abcdefhi")))))
        (parameterize ([plot-height (- (plot-height)
                                       (if (gr-title) fs 0)
                                       (if (gr-x-label) fs 0))]
                       [plot-width (- (plot-width)
                                      (if (gr-y-label) fs 0))])
          (cond [(hash-ref mapping 'facet #f) (facet-plot render-fns facet-wrap)]
                [else
                 (define-values (left right bottom top) (plot-extras-size metric-plot))
                 (add-all-titles (graph-internal render-fns)
                                 #:x-offset (- (+ right left
                                                  (if (gr-y-label) fs 0)))
                                 #:y-offset (- (+ top bottom
                                                  (if (xor (gr-title) (gr-x-label)) fs 0))))]))))))

(define (save-pict pict path)
  (define ext (path-get-extension path))
  (void
   (match ext
     [(or #".png" #".pdf" #".svg")
      (with-output-to-file path
        (λ () (write-bytes (convert pict
                                    (string->symbol
                                     (string-append (bytes->string/utf-8 (subbytes ext 1))
                                                    "-bytes"))))))]
     [_ (error 'save-pict "unsupported extension")])))

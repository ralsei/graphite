#lang debug racket/base
(require data-frame
         fancy-app
         file/convertible
         pict
         (except-in plot/no-gui
                    density
                    error-bars
                    lines
                    points)
         plot/utils
         racket/contract/base
         racket/format
         racket/function
         racket/list
         racket/match
         racket/math
         racket/path
         racket/vector
         "aes.rkt"
         "bar.rkt"
         "boxplot.rkt"
         "col.rkt"
         "density.rkt"
         "error-bars.rkt"
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

; XXX: should we support multiple facets? n facets?
(define (facet-plot render-fns wrap)
  (define facet (hash-ref (gr-global-mapping) 'facet))
  (define groups (possibilities (gr-data) facet))
  (define facet-wrap (or wrap (inexact->exact (ceiling (sqrt (vector-length groups))))))
  (define wrapped-groups (vector-reshape groups facet-wrap))
  (define num-blanks (vector-count not (vector-ref wrapped-groups
                                                   (sub1 (vector-length wrapped-groups)))))

  (define metrics-plot
    (parameterize ([gr-global-mapping (hash-remove (gr-global-mapping) 'facet)])
      (graph-internal #f render-fns)))
  (match-define (vector (vector x-min x-max)
                        (vector y-min y-max))
    (plot-pict-bounds metrics-plot))
  (define-values (left right bot top) (plot-extras-size metrics-plot))

  (define title-height (pict-height (title "abcdefg")))

  ; p cols x q rows
  (define grid-p (vector-length wrapped-groups))
  (define grid-q (vector-length (vector-ref wrapped-groups 0)))

  (define width (inexact->exact (round (/ (- (plot-width) left (* grid-q right))
                                          grid-q))))
  ; FIXME: figure out why height is sporadic
  (define height (inexact->exact (round (/ (- (plot-height) bot top
                                              (* grid-p title-height) title-height)
                                           grid-p))))

  (define (run-plot group [with-x-extras? #f] [with-y-extras? #f])
    (parameterize ([plot-x-ticks (if with-x-extras? (plot-x-ticks) no-ticks)]
                   [gr-add-x-ticks? with-x-extras?]
                   [plot-x-label (and with-x-extras? (plot-x-label))]
                   [plot-y-ticks (if with-y-extras? (plot-y-ticks) no-ticks)]
                   [gr-add-y-ticks? with-y-extras?]
                   [plot-y-label (and with-y-extras? (plot-y-label))]
                   [gr-facet-label group])
      (if group
          (add-facet-label
           (plot-with-area (thunk (graph-internal group render-fns)) width height))
          (background-rectangle width (+ height bot top))))) ; only appears at the bottom

  (define (plot-row group-vector [with-x-extras? #f] [end-add-x? #f])
    (for/fold ([plt (blank)])
              ([(grp idx) (in-indexed (in-vector group-vector))])
      (define add-x-extras? (or with-x-extras?
                                (and end-add-x? (< (abs (- grid-q (add1 idx))) num-blanks))))
      (hb-append plt
                 (let ([v (run-plot grp add-x-extras? (zero? idx))])
                   (if (and end-add-x? (not add-x-extras?)) (inset v 0 0 0 bot) v)))))

  (define almost
    (parameterize ([gr-x-min (if (not (gr-x-min)) x-min (gr-x-min))]
                   [gr-x-max (if (not (gr-x-max)) x-max (gr-x-max))]
                   [gr-y-min (if (not (gr-y-min)) y-min (gr-y-min))]
                   [gr-y-max (if (not (gr-y-max)) y-max (gr-y-max))])
      (for/fold ([plt (blank)])
                ([(grp-vector idx) (in-indexed (in-vector wrapped-groups))])
        (define offset (- (sub1 grid-p) idx))
        (cond [(zero? offset)
               (vl-append-backwards (if (not (zero? num-blanks)) (- bot) 0)
                                    plt
                                    (plot-row grp-vector #t))]
              [(= offset 1)
               (vl-append plt (plot-row grp-vector #f #t))]
              [else (vl-append plt (plot-row grp-vector))]))))

  (cc-superimpose (background-rectangle (pict-width almost)
                                        (pict-height almost))
                  almost))

(define (graph-internal group render-fns)
  (plot-pict #:x-min (gr-x-min)
             #:x-max (gr-x-max)
             #:y-min (gr-y-min)
             #:y-max (gr-y-max)
             (parameterize ([gr-group group])
               (for/list ([render-fn (in-list render-fns)])
                 (render-fn)))))

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
    (match-define (vector (vector actual/x-min actual/x-max)
                          (vector actual/y-min actual/y-max))
      ; bare minimum params, don't facet
      (parameterize ([plot-pen-color-map 'set1]
                     [plot-brush-color-map 'set1]
                     [gr-global-mapping (hash-remove (gr-global-mapping) 'facet)])
        (plot-pict-bounds (graph-internal #f render-fns))))

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
          (add-all-titles
           (cond [(hash-ref mapping 'facet #f) (facet-plot render-fns facet-wrap)]
                 [else (graph-internal #f render-fns)])))))))

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

#lang racket
(require pict plot/pict
         "fit.rkt"
         "points.rkt")
(provide pplot aes save-pict
         (all-from-out "fit.rkt")
         (all-from-out "points.rkt"))

(define keyword->symbol (compose string->symbol keyword->string))

(define aes
  (make-keyword-procedure
   (λ (kws kw-args . rst)
     (when (not (empty? rst))
       (error "aes called with non-keyword argument"))
     (for/hash ([kw (in-list kws)]
                [kwa (in-list kw-args)])
       (values (keyword->symbol kw) kwa)))))

(define (pplot #:data data #:mapping mapping
               #:title [title (plot-title)]
               #:x-label [x-label (plot-x-label)]
               #:x-transform [x-transform (plot-x-transform)]
               #:x-ticks [x-ticks (plot-x-ticks)]
               #:x-conv [x-conv id-function]
               #:y-label [y-label (plot-y-label)]
               #:y-transform [y-transform (plot-y-transform)]
               #:y-ticks [y-ticks (plot-y-ticks)]
               #:y-conv [y-conv id-function]
               . render-fns)
  (parameterize ([plot-title title]
                 [plot-x-label x-label]
                 [plot-y-label y-label]
                 [plot-x-transform x-transform]
                 [plot-x-ticks x-ticks]
                 [plot-y-transform y-transform]
                 [plot-y-ticks y-ticks]
                 ; better defaults
                 [plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [plot-font-face "Arial"]
                 [point-sym 'bullet]
                 [plot-pen-color-map (hash-ref mapping 'colormap 'set1)])
    ; (define facet-x (hash-ref mapping 'facet-x #f))
    ; (define facet-y (hash-ref mapping 'facet-y #f))
    (plot
     (for/list ([render-fn (in-list render-fns)])
       (render-fn #:data data
                  #:gmapping mapping ; "global mapping"
                  #:x-conv x-conv
                  #:y-conv y-conv)))))

(define (save-pict pict path)
  (send (pict->bitmap pict) save-file path 'png))

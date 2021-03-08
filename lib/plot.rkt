#lang racket
(require pict plot/pict
         "bar.rkt"
         "fit.rkt"
         "points.rkt")
(provide pplot aes save-pict
         (all-from-out "bar.rkt")
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
               #:width [width (plot-width)]
               #:height [height (plot-height)]
               #:title [title (plot-title)]
               #:x-label [x-label (plot-x-label)]
               #:x-transform [x-transform (plot-x-transform)]
               #:x-ticks [x-ticks (plot-x-ticks)]
               #:x-conv [x-conv id-function]
               #:x-conv-ticks? [x-conv-ticks? #f]
               #:y-label [y-label (plot-y-label)]
               #:y-transform [y-transform (plot-y-transform)]
               #:y-ticks [y-ticks (plot-y-ticks)]
               #:y-conv [y-conv id-function]
               #:y-conv-ticks? [y-conv-ticks? #f]
               . render-fns)
  (parameterize ([plot-title title]
                 [plot-width width]
                 [plot-height height]
                 [plot-x-label x-label]
                 [plot-y-label y-label]
                 [plot-x-transform x-transform]
                 [plot-x-ticks
                  (if x-conv-ticks?
                      (ticks-scale x-ticks (invertible-inverse x-conv))
                      x-ticks)]
                 [plot-y-transform y-transform]
                 [plot-y-ticks
                  (if y-conv-ticks?
                      (ticks-scale y-ticks (invertible-inverse y-conv))
                      y-ticks)]
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
                  #:x-conv (invertible-function-f x-conv)
                  #:y-conv (invertible-function-f y-conv))))))

(define (save-pict pict path)
  (match (path-get-extension path)
    [(or #".png" #".pdf" #".svg")
     (with-output-to-file path
       (lambda () (write-bytes (convert pict
                                        (string->symbol
                                         (string-append (bytes->string/utf-8 (subbytes ext 1))
                                                        "-bytes"))))))]
    [_ (error 'save-pict "unsupported extension")]))

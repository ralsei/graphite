#lang racket
(require racket/hash
         data-frame plot fancy-app
         plot/utils)
(provide (all-defined-out))

; dataframe : [A B C D] dataframe string string (A -> B) (C -> D) -> renderer2d
; given a dataframe, the data to use for the x-axis, the data to use
; for the y-axis, and conversion functions for both, plot it
(define (dataframe df x-axis y-axis
                   #:x-conv [x-conv values] #:y-conv [y-conv values]
                   #:color [color 'black] #:label [label #f])
  (points
   (for/vector ([(x y) (in-data-frame df x-axis y-axis)]
                #:when (and x y))
     (vector (x-conv x) (y-conv y)))
   #:color color #:label label))

(define ((ppoints #:mapping [local-mapping (make-hash)])
         data mapping x-conv y-conv)
  ; overwrite using the local mapping
  (define aes (hash-union mapping local-mapping #:combine (λ (x y) x)))
  (define discrete-color (hash-ref aes 'discrete-color #f))
  (cond [discrete-color
         (define tbl (make-hash))
         (for ([(x y strat) (in-data-frame data
                                           (hash-ref aes 'x)
                                           (hash-ref aes 'y)
                                           discrete-color)]
               #:when (and x y))
           (hash-update! tbl strat (cons (vector (x-conv x) (y-conv y)) _) null))

         (let ([color-n -1])
           (hash-map tbl
                     (λ (strat pts)
                       (set! color-n (add1 color-n))
                       (points pts #:color (->pen-color color-n) #:label strat))
                     #t))]
        [else
         (points
          (for/vector ([(x y) (in-data-frame data (hash-ref aes 'x) (hash-ref aes 'y))]
                       #:when (and x y))
            (vector (x-conv x) (y-conv y))))]))

(define ((fit #:method [method 'linear] #:mapping [local-mapping (make-hash)]
              #:poly-degree [polynomial-degree 2])
         data mapping x-conv y-conv)
  (define aes (hash-union mapping local-mapping #:combine (λ (x y) x)))
  (define fit-line (df-least-squares-fit data
                                         (hash-ref aes 'x) (hash-ref aes 'y)
                                         #:mode method #:polynomial-degree polynomial-degree))
  (function fit-line #:width (hash-ref aes 'width 1)))


(define (pplot #:data data #:mapping mapping
               #:x-transform [x-transform (plot-x-transform)]
               #:x-ticks [x-ticks (plot-x-ticks)]
               #:x-conv [x-conv values]
               #:y-transform [y-transform (plot-y-transform)]
               #:y-ticks [y-ticks (plot-y-ticks)]
               #:y-conv [y-conv values]
               . render-fns)
  (parameterize ([plot-title (hash-ref mapping 'title (plot-title))]
                 [plot-x-label (hash-ref mapping 'x-label (plot-x-label))]
                 [plot-y-label (hash-ref mapping 'y-label (plot-y-label))]
                 [plot-x-transform x-transform]
                 [plot-x-ticks x-ticks]
                 [plot-y-transform y-transform]
                 [plot-y-ticks y-ticks]
                 ; better defaults
                 [plot-font-face "Arial"]
                 [point-sym 'bullet]
                 [plot-pen-color-map (hash-ref mapping 'colormap 'set1)])
    (plot
     (for/list ([render-fn (in-list render-fns)])
       (render-fn data mapping x-conv y-conv)))))

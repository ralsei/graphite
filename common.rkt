#lang racket
(require racket/hash racket/set
         bestfit data-frame fancy-app pict plot/pict plot/utils threading)
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

(define keyword->symbol (compose string->symbol keyword->string))

(define aes
  (make-keyword-procedure
   (λ (kws kw-args . rst)
     (when (not (empty? rst))
       (error "aes called with non-keyword argument"))
     (for/hash ([kw (in-list kws)]
                [kwa (in-list kw-args)])
       (values (keyword->symbol kw) kwa)))))

(define ((ppoints #:mapping [local-mapping (make-hash)])
         data mapping x-conv y-conv)
  ; overwrite using the local mapping
  (define aes (hash-union mapping local-mapping #:combine (λ (x y) x)))
  (define discrete-color (hash-ref aes 'discrete-color #f))
  (define alpha (hash-ref aes 'alpha 1))
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
                       (points pts #:color (->pen-color color-n) #:label strat #:alpha alpha))
                     #t))]
        [else
         (points
          (for/vector ([(x y) (in-data-frame data (hash-ref aes 'x) (hash-ref aes 'y))]
                       #:when (and x y))
            (vector (x-conv x) (y-conv y)))
          #:alpha alpha)]))

(define ((fit #:method [method 'linear] #:mapping [local-mapping (make-hash)])
         data mapping x-conv y-conv)
  (define aes (hash-union mapping local-mapping #:combine (λ (x y) x)))
  (define fit-function
    (match method
      ['linear linear-fit]
      ['exp exp-fit]
      ['power power-fit]
      ['log log-fit]))
  (define fit-line
    (for/fold ([xs '()] [ys '()]
               #:result (fit-function xs ys))
              ([(x y) (in-data-frame data (hash-ref aes 'x) (hash-ref aes 'y))]
               #:when (and x y))
      (values (cons (exact->inexact (x-conv x)) xs) (cons (exact->inexact (y-conv y)) ys))))
  (function fit-line #:width (hash-ref aes 'width 1)))

(define (vector-remove-duplicates vec)
  (define seen (mutable-set))
  (for/vector ([v (in-vector vec)]
               #:unless (set-member? seen v))
    (set-add! seen v)
    v))

(define (possibilities data group)
  (~> (df-select data group)
      vector-remove-duplicates
      (vector-filter (λ (x) (and x #t)) _)))

(define (bar-stratify data mode mapping)
  (define strats (possibilities data (hash-ref mapping 'group)))
  (for/list ([s (in-vector strats)]
             [i (in-naturals)])
    ((bar #:mode mode) data mapping values values (+ (vector-length strats) 0.5) i s)))

(define ((bar #:mode [mode 'count] #:mapping [local-mapping (make-hash)])
         data mapping x-conv y-conv [skip (discrete-histogram-skip)] [x-min 0] [grp #f])
  (define aes (hash-union mapping local-mapping #:combine (λ (x y) x)))
  (cond [(and (not grp) (hash-ref aes 'group #f)) (bar-stratify data mode aes)]
        [else
         (define count-tbl (make-hash))
         (cond [grp
                (for ([(x strat) (in-data-frame data (hash-ref aes 'x) (hash-ref aes 'group))]
                      #:when (and x (equal? strat grp)))
                  (hash-update! count-tbl x add1 1))]
               [else
                (for ([(x) (in-data-frame data (hash-ref aes 'x))]
                      #:when x)
                  (hash-update! count-tbl x add1 1))])

         (define tbl
           (match mode
             ['count count-tbl]
             ['prop (define total (for/sum ([(_ v) (in-hash count-tbl)]) v))
                    (for/hash ([(k c) (in-hash count-tbl)])
                      (values k (/ c total)))]))
         (discrete-histogram
          #:skip skip #:x-min x-min #:label grp
          (for/vector ([(var cnt) (in-hash tbl)])
            (vector var cnt)))]))

(define (pplot #:data data #:mapping mapping
               #:title [title (plot-title)]
               #:x-label [x-label (plot-x-label)]
               #:x-transform [x-transform (plot-x-transform)]
               #:x-ticks [x-ticks (plot-x-ticks)]
               #:x-conv [x-conv values]
               #:y-label [y-label (plot-y-label)]
               #:y-transform [y-transform (plot-y-transform)]
               #:y-ticks [y-ticks (plot-y-ticks)]
               #:y-conv [y-conv values]
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
       (render-fn data mapping x-conv y-conv)))))

(define (save-pict pict path)
  (send (pict->bitmap pict) save-file path 'png))

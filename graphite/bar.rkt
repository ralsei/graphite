#lang racket
(require fancy-app pict plot/pict plot/utils
         "contracts.rkt" "util.rkt")
(provide
 (contract-out
  [bar (->* ()
            (#:mode (or/c 'count 'prop)
             #:mapping (aes-containing/c #:x string?
                                         #:facet (or/c string? #f)
                                         #:group any/c
                                         #:group-gap (>=/c 0)
                                         #:x-min (or/c rational? #f)
                                         #:x-max (or/c rational? #f)
                                         #:y-min (or/c rational? #f)
                                         #:y-max (or/c rational? #f)
                                         #:gap (real-in 0 1)
                                         #:skip (>=/c 0)
                                         #:invert? boolean?
                                         #:color plot-color/c
                                         #:style plot-brush-style/c
                                         #:line-color plot-color/c
                                         #:line-width (>=/c 0)
                                         #:line-style plot-pen-style/c
                                         #:alpha (real-in 0 1)
                                         #:label (or/c string? pict? #f)
                                         #:add-ticks? boolean?
                                         #:far-ticks? boolean?))
            graphite-renderer?)]
  [stacked-bar (->* ()
                    (#:mode (or/c 'count 'prop)
                     #:mapping (aes-containing/c #:x string?
                                                 #:facet (or/c string? #f)
                                                 #:group string?
                                                 #:x-min (or/c rational? #f)
                                                 #:x-max (or/c rational? #f)
                                                 #:y-min (or/c rational? #f)
                                                 #:y-max (or/c rational? #f)
                                                 #:gap (real-in 0 1)
                                                 #:skip (>=/c 0)
                                                 #:invert? boolean?
                                                 #:colors (plot-colors/c nat/c)
                                                 #:styles (plot-brush-styles/c nat/c)
                                                 #:line-colors (plot-colors/c nat/c)
                                                 #:line-widths (plot-colors/c nat/c)
                                                 #:line-styles (plot-pen-styles/c nat/c)
                                                 #:alphas (alphas/c nat/c)
                                                 #:labels (labels/c nat/c)
                                                 #:add-ticks? boolean?
                                                 #:far-ticks? boolean?))
                    graphite-renderer?)]))

(define (make-count-table mode group)
  (define count-tbl (make-hash))
  (for ([(x strat facet) (in-data-frame* (gr-data) (hash-ref (gr-global-mapping) 'x)
                                         (hash-ref (gr-global-mapping) 'group #f)
                                         (hash-ref (gr-global-mapping) 'facet #f))]
        #:when x
        #:when (equal? strat group)
        #:when (equal? facet (gr-group)))
    (hash-update! count-tbl ((gr-x-conv) x) add1 1))

  (match mode
    ['count count-tbl]
    ['prop (define total (for/sum ([(_ v) (in-hash count-tbl)]) v))
           (for/hash ([(k c) (in-hash count-tbl)])
             (values k (/ c total)))]))

(define (bar-dodged #:mode mode)
  (define strats (possibilities (gr-data) (hash-ref (gr-global-mapping) 'group)))
  (for/list ([var (in-vector strats)]
             [i (in-naturals)])
    (parameterize ([rectangle-color (->pen-color i)])
      (bar-simple #:skip (+ (vector-length strats) (hash-ref (gr-global-mapping) 'group-gap 1))
                  #:x-min i
                  #:group var
                  #:mode mode))))

(define (bar-simple #:mode mode #:skip [skip (discrete-histogram-skip)]
                    #:x-min [x-min 0] #:group [group #f])
  (define tbl (make-count-table mode group))

  (run-renderer
   #:renderer discrete-histogram
   #:mapping (gr-global-mapping)
   #:skip skip #:x-min x-min #:label group
   (for/vector ([(var cnt) (in-hash tbl)])
     (vector var cnt))))

(define-renderer (bar #:mode [mode 'count] #:mapping [local-mapping (make-hash)])
                 (#:y-label (symbol->string mode))
  (parameterize ([gr-global-mapping (mapping-override (gr-global-mapping) local-mapping)])
    (cond [(hash-ref (gr-global-mapping) 'group #f) (bar-dodged #:mode mode)]
          [else (bar-simple #:mode mode)])))

(define-renderer (stacked-bar #:mode [mode 'count] #:mapping [local-mapping (make-hash)])
                 (#:y-label (symbol->string mode))
  (define aes (mapping-override (gr-global-mapping) local-mapping))

  ; first generate every table based on every group...
  (define strats (possibilities (gr-data) (hash-ref aes 'group)))
  (define tables
    (parameterize ([gr-global-mapping aes])
      (for/list ([group (in-vector strats)])
        (make-count-table mode group))))

  ; then look up each variable
  (define xs (possibilities (gr-data) (hash-ref aes 'x)))
  (define to-plot
    (for/list ([x (in-vector xs)])
      (vector x (for/list ([tbl (in-list tables)])
                  (hash-ref tbl x 0)))))

  (run-renderer #:renderer stacked-histogram
                #:mapping (gr-global-mapping)
                #:labels (vector->list strats)
                to-plot))

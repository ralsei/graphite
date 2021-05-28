#lang racket/base
(require fancy-app
         pict
         plot/no-gui
         plot/utils
         racket/contract/base
         racket/match
         "aes.rkt"
         "renderer.rkt"
         "util.rkt")

(provide
 (contract-out
  [bar (->* ()
            (#:x-min (or/c rational? #f)
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
             #:far-ticks? boolean?
             #:mode (or/c 'count 'prop)
             #:group-gap (>=/c 0)
             #:mapping (aes-containing/c #:x string?
                                         #:facet (or/c string? #f)
                                         #:group (or/c string? #f)))
            graphite-renderer?)]
  [stacked-bar (->* ()
                    (#:x-min (or/c rational? #f)
                     #:x-max (or/c rational? #f)
                     #:y-min (or/c rational? #f)
                     #:y-max (or/c rational? #f)
                     #:gap (real-in 0 1)
                     #:skip (>=/c 0)
                     #:invert? boolean?
                     #:colors (plot-colors/c nat/c)
                     #:styles (plot-brush-styles/c nat/c)
                     #:line-colors (plot-colors/c nat/c)
                     #:line-widths (pen-widths/c nat/c)
                     #:line-styles (plot-pen-styles/c nat/c)
                     #:alphas (alphas/c nat/c)
                     #:labels (labels/c nat/c)
                     #:add-ticks? boolean?
                     #:far-ticks? boolean?
                     #:mode (or/c 'count 'prop)
                     #:mapping (aes-containing/c #:x string?
                                                 #:facet (or/c string? #f)
                                                 #:group string?))
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

(define (bar-dodged #:mode mode #:group-gap group-gap #:kws kws #:kw-args kw-args)
  (define strats (possibilities (gr-data) (hash-ref (gr-global-mapping) 'group)))
  (for/list ([var (in-vector strats)]
             [i (in-naturals)])
    (parameterize ([rectangle-color (->brush-color i)])
      (bar-simple #:skip (+ (vector-length strats) group-gap)
                  #:x-min i
                  #:group var
                  #:mode mode
                  #:kws kws #:kw-args kw-args))))

(define (bar-simple #:mode mode #:skip [skip (discrete-histogram-skip)]
                    #:x-min [x-min 0] #:group [group #f]
                    #:kws kws #:kw-args kw-args)
  (define tbl (make-count-table mode group))

  (run-renderer
   #:renderer discrete-histogram
   #:kws kws #:kw-args kw-args
   #:skip skip #:x-min x-min #:label group
   #:add-ticks? (not (equal? (plot-x-ticks) no-ticks))
   (for/vector ([(var cnt) (in-hash tbl)])
     (vector var cnt))))

(define-renderer (bar #:kws kws #:kw-args kw-args
                      #:group-gap [group-gap 1]
                      #:mode [mode 'count] #:mapping [local-mapping (aes)])
                 (#:y-label (symbol->string mode))
  (parameterize ([gr-global-mapping (mapping-override (gr-global-mapping) local-mapping)])
    (cond [(hash-ref (gr-global-mapping) 'group #f) (bar-dodged #:mode mode #:group-gap group-gap
                                                                #:kws kws #:kw-args kw-args)]
          [else (bar-simple #:mode mode
                            #:kws kws #:kw-args kw-args)])))

(define-renderer (stacked-bar #:kws kws #:kw-args kw-args
                              #:mode [mode 'count] #:mapping [local-mapping (aes)])
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
                #:kws kws #:kw-args kw-args
                #:labels (vector->list strats)
                #:add-ticks? (not (equal? (plot-x-ticks) no-ticks))
                to-plot))

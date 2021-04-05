#lang racket
(require fancy-app plot/pict plot/utils "util.rkt")
(provide bar stacked-bar)

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

(define ((bar #:mode [mode 'count] #:mapping [local-mapping (make-hash)]))
  (parameterize ([gr-global-mapping (mapping-override (gr-global-mapping) local-mapping)])
    (cond [(hash-ref (gr-global-mapping) 'group #f) (bar-dodged #:mode mode)]
          [else (bar-simple #:mode mode)])))

(define ((stacked-bar #:mode [mode 'count] #:mapping [local-mapping (make-hash)]))
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

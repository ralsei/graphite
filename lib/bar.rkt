#lang racket
(require fancy-app plot/pict plot/utils "util.rkt")
(provide bar)

(define (bar-dodged #:data data #:mode mode #:mapping mapping #:x-conv x-conv
                    #:group-by grp #:facet-group group)
  (define strats (possibilities data (hash-ref mapping 'group)))
  (for/list ([var (in-vector strats)]
             [i (in-naturals)])
    (parameterize ([rectangle-color (->pen-color i)])
      (bar-simple #:data data #:mode mode #:mapping mapping
                  #:skip (+ (vector-length strats) (hash-ref mapping 'group-gap 1))
                  #:x-min i
                  #:group var
                  #:facet-group group))))

(define (bar-simple #:data data #:mode mode #:mapping mapping #:x-conv x-conv
                    #:skip [skip (discrete-histogram-skip)] #:x-min [x-min 0]
                    #:group [group #f] #:facet-group facet-group)
  (define count-tbl (make-hash))
  (for ([(x strat facet) (in-data-frame* data (hash-ref mapping 'x)
                                         (hash-ref mapping 'group #f)
                                         (hash-ref mapping 'facet #f))]
        #:when x
        #:when (if group (equal? strat group) #t)
        #:when (if facet-group (equal? facet facet-group) #t))
    (hash-update! count-tbl (x-conv x) add1 1))

  (define tbl
    (match mode
      ['count count-tbl]
      ['prop (define total (for/sum ([(_ v) (in-hash count-tbl)]) v))
             (for/hash ([(k c) (in-hash count-tbl)])
               (values k (/ c total)))]))

  (discrete-histogram
   #:skip skip #:x-min x-min #:label group
   (for/vector ([(var cnt) (in-hash tbl)])
     (vector var cnt))))

(define ((bar #:mode [mode 'count] #:mapping [local-mapping (make-hash)])
         #:data data #:gmapping mapping #:x-conv x-conv #:y-conv y-conv #:group group)
  (define aes (mapping-override mapping local-mapping))
  (cond [(hash-ref aes 'group #f) (bar-dodged #:data data #:mode mode
                                              #:x-conv x-conv #:mapping aes
                                              #:facet-group group)]
        [else (bar-simple #:data data #:mode mode #:x-conv x-conv
                          #:mapping aes #:facet-group group)]))

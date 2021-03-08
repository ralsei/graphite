#lang racket
(require racket/hash data-frame fancy-app plot/pict plot/utils "util.rkt")
(provide bar)

(define (bar-dodged #:data data #:mode mode #:mapping mapping #:group-by grp)
  (define strats (possibilities data grp))
  (for/list ([var (in-vector strats)]
             [i (in-naturals)])
    (parameterize ([rectangle-color (->pen-color i)])
      (bar-simple #:data data #:mode mode #:mapping mapping
                  #:skip (+ (vector-length strats) (hash-ref mapping 'group-gap 1))
                  #:x-min i
                  #:group var))))

(define (bar-simple #:data data #:mode mode #:mapping mapping
                    #:skip [skip (discrete-histogram-skip)] #:x-min [x-min 0] #:group [group #f])
  (define count-tbl (make-hash))
  (cond [group (for ([(x strat) (in-data-frame data (hash-ref mapping 'x) (hash-ref mapping 'group))]
                     #:when (and x (equal? strat group)))
                 (hash-update! count-tbl x add1 1))]
        [else (for ([(x) (in-data-frame data (hash-ref mapping 'x))]
                    #:when x)
                (hash-update! count-tbl x add1 1))])

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
         #:data data #:gmapping mapping #:x-conv x-conv #:y-conv y-conv)
  (define aes (hash-union mapping local-mapping #:combine (λ (x y) x)))
  (cond [(hash-ref aes 'group #f) => (bar-dodged #:data data #:mode mode
                                                 #:mapping aes #:group-by _)]
        [else (bar-simple #:data data #:mode mode #:mapping aes)]))

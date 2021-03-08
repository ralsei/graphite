#lang racket
(require racket/hash
         data-frame fancy-app plot/pict plot/utils)
(provide ppoints)

(define ((ppoints #:mapping [local-mapping (make-hash)])
         #:data data #:gmapping mapping #:x-conv x-conv #:y-conv y-conv)
  (define aes (hash-union mapping local-mapping #:combine (λ (x y) x)))
  (define discrete-color (hash-ref aes 'discrete-color #f))
  (define alpha (hash-ref aes 'alpha 1))

  (define tbl (make-hash))
  (cond [discrete-color
         (for ([(x y strat) (in-data-frame data
                                           (hash-ref aes 'x)
                                           (hash-ref aes 'y)
                                           discrete-color)]
               #:when (and x y))
           (hash-update! tbl strat (cons (vector (x-conv x) (y-conv y)) _) null))]
        [else
         (for ([(x y) (in-data-frame data (hash-ref aes 'x) (hash-ref aes 'y))]
               #:when (and x y))
           (hash-update! tbl #f (cons (vector (x-conv x) (y-conv y)) _) null))])

  (let ([color-n -1])
    (hash-map tbl
              (λ (strat pts)
                (set! color-n (add1 color-n))
                (points pts #:color (->pen-color color-n) #:label strat #:alpha alpha))
              #t)))

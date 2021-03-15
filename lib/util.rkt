#lang racket
(require data-frame threading racket/hash)
(provide (all-defined-out))

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

(define (mapping-override mapping local-mapping)
  (hash-union mapping local-mapping #:combine (λ (x y) x)))

(define (in-infinite val)
  (in-cycle (in-value val)))

(define (in-data-frame* data . series)
  (define generators
    (for/list ([s (in-list series)])
      (cond [s (in-data-frame data s)]
            [else (in-infinite s)])))

  (cond [(empty? generators) (in-parallel '())]
        [else (apply in-parallel generators)]))

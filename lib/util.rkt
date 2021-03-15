#lang racket
(require data-frame threading racket/hash)
(provide vector-remove-duplicates possibilities mapping-override in-infinite)

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
  3)

#lang racket
(require data-frame graphite plot/utils rackunit
         racket/runtime-path
         "util.rkt")
(provide (all-defined-out))

(define-runtime-path points-1-data "./test-data/points-1.dat")
(define points-1-df
  (let ([int-data (make-data-frame)]
        [xs (build-vector 10000 add1)])
    (df-add-series! int-data (make-series "x-var" #:data xs))
    (df-add-series! int-data (make-series "y-var" #:data xs))
    int-data))
(define points-1
  (graph #:data points-1-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "A line"
         (points)))

(define-runtime-path points-2-data "./test-data/points-2.dat")
(define points-2-df
  (begin
    (random-seed 42)
    (let ([int-data (make-data-frame)]
          [xs (build-vector 10000 (λ (_) (random)))]
          [ys (build-vector 10000 (λ (_) (random)))])
      (df-add-series! int-data (make-series "x-var" #:data xs))
      (df-add-series! int-data (make-series "y-var" #:data ys))
      int-data)))
(define points-2
  (graph #:data points-2-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "Random data, redux"
         (points)))

(module+ test
  (check-draw-steps points-1 points-1-data)
  (check-draw-steps points-2 points-2-data))

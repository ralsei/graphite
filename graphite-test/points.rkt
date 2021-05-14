#lang racket
(require data-frame graphite plot/utils rackunit
         racket/runtime-path
         "util.rkt")

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

(define-runtime-path points-3-data "./test-data/points-3.dat")
(define points-3
  (graph #:data points-1-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:x-transform logarithmic-transform
         #:title "A log transformed line"
         (points)))

(define-runtime-path points-4-data "./test-data/points-4.dat")
(define points-4
  (graph #:data points-1-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:x-conv (λ (x) (+ 20000 x))
         #:title "A converted line"
         (points)))

(define-runtime-path points-5-data "./test-data/points-5.dat")
(define points-5-df
  (begin
    (random-seed 868)
    (let ([int-data (make-data-frame)]
          [xs (build-vector 1000 (λ (_) (* (random) 30)))]
          [ys (build-vector 1000 (λ (_) (* (random) 30)))]
          [strats (for/vector ([n (in-range 0 1000)]
                               [x (in-cycle (in-list '("a" "b" "c" "d" "e")))])
                    x)])
      (df-add-series! int-data (make-series "x-var" #:data xs))
      (df-add-series! int-data (make-series "y-var" #:data ys))
      (df-add-series! int-data (make-series "stratify-on" #:data strats))
      int-data)))
(define points-5
  (graph #:data points-5-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "Happy birthday!"
         (points #:mapping (aes #:discrete-color "stratify-on"))))

(module+ test
  (check-draw-steps points-1 points-1-data)
  (check-draw-steps points-2 points-2-data)
  (check-draw-steps points-3 points-3-data)
  (check-draw-steps points-4 points-4-data)
  (check-draw-steps points-5 points-5-data))

#lang racket
(require data-frame graphite rackunit racket/runtime-path
         (except-in plot/no-gui density lines points)
         (prefix-in plot: (only-in plot/no-gui density lines points))
         "util.rkt")
(provide (all-defined-out))

(define random-df
  (begin
    (random-seed 96)
    (let ([int-data (make-data-frame)]
          [xs (build-vector 10000 (λ (_) (random)))]
          [ys (build-vector 10000 (λ (_) (random)))])
      (df-add-series! int-data (make-series "x-var" #:data xs))
      (df-add-series! int-data (make-series "y-var" #:data ys))
      (df-add-derived! int-data "log-x" '("x-var") (λ (x) (log (first x) 10)))
      (df-add-derived! int-data "log-y" '("y-var") (λ (y) (log (first y) 10)))
      (df-add-derived! int-data "exp-x" '("x-var") (λ (x) (expt 10 (first x))))
      (df-add-derived! int-data "exp-y" '("y-var") (λ (y) (expt 10 (first y))))
      int-data)))

(define-runtime-path transform-1-data "./test-data/transform-1.dat")
(define transform-1
  (parameterize ([plot-x-ticks (get-adjusted-ticks logarithmic-transform)]
                 [plot-y-ticks (get-adjusted-ticks no-transform)])
    (plot-pict (plot:points (df-select* random-df "exp-x" "y-var")))))
(define-runtime-path transform-1.v2-data "./test-data/transform-1.v2.dat")
(define transform-1.v2
  (parameterize ([plot-x-ticks (get-adjusted-ticks logarithmic-transform)])
    (plot-pict (plot:points (df-select* random-df "exp-x" "y-var")))))

(define-runtime-path transform-2-data "./test-data/transform-2.dat")
(define transform-2
  (parameterize ([plot-x-ticks (get-adjusted-ticks (only-ticks no-ticks))]
                 [plot-y-ticks (get-adjusted-ticks (only-ticks no-ticks))])
    (plot-pict (plot:points (df-select* random-df "x-var" "y-var")))))
(define-runtime-path transform-2.v2-data "./test-data/transform-2.v2.dat")
(define transform-2.v2
  (parameterize ([plot-x-ticks no-ticks]
                 [plot-y-ticks no-ticks])
    (plot-pict (plot:points (df-select* random-df "x-var" "y-var")))))

(define-runtime-path transform-3-data "./test-data/transform-3.dat")
(define transform-3
  (graph #:data random-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:x-transform logarithmic-transform
         (points)))

(define-runtime-path transform-4-data "./test-data/transform-4.dat")
(define transform-4-df
  (begin
    (random-seed 402)
    (let ([int-data (make-data-frame)]
          [xs (build-vector 100 (λ (_) (integer->char (random 0 55295))))]
          [ys (build-vector 100 (λ (_) (random)))])
      (df-add-series! int-data (make-series "chars" #:data xs))
      (df-add-series! int-data (make-series "ints" #:data ys))
      int-data)))
(define transform-4
  (graph #:data transform-4-df
         #:mapping (aes #:x "chars" #:y "ints")
         #:x-conv char->integer
         #:x-transform logarithmic-transform
         (points)))

(define-runtime-path transform-5-data "./test-data/transform-5.dat")
(define transform-5
  (graph #:data transform-4-df
         #:mapping (aes #:x "ints" #:y "chars")
         #:y-conv char->integer
         #:y-transform logarithmic-transform
         (points)))

(module+ test
  (check-draw-steps transform-1 transform-1-data)
  (check-draw-steps transform-1.v2 transform-1.v2-data)
  (check-same-draw-steps? transform-1-data transform-1.v2-data)

  (check-draw-steps transform-2 transform-2-data)
  (check-draw-steps transform-2.v2 transform-2.v2-data)
  (check-same-draw-steps? transform-2-data transform-2.v2-data)

  (check-draw-steps transform-3 transform-3-data)

  (check-draw-steps transform-4 transform-4-data)
  (check-draw-steps transform-5 transform-5-data))

#lang racket
(require data-frame graphite rackunit
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

(define-runtime-path lines-1-data "./test-data/lines-1.dat")
(define lines-1
  (graph #:data points-5-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "Adversarial lines"
         (lines #:mapping (aes #:discrete-color "stratify-on"))))

(define-runtime-path lines-2-data "./test-data/lines-2.dat")
(define lines-2-df
  (let ([int-data (make-data-frame)])
    (df-add-series! int-data (make-series "x-var" #:data (build-vector 1000 identity)))
    (df-add-series! int-data
                    (make-series "y-var" #:data (build-vector 1000 (λ (x) (+ x (* (random) 20))))))
    int-data))
(define lines-2
  (graph #:data lines-2-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "Random walk"
         (lines)))

(define-runtime-path fit-1-data "./test-data/fit-1.dat")
(define fit-1-df
  (let ([int-data (make-data-frame)])
    (df-add-series! int-data (make-series "x-var" #:data (build-vector 1000 identity)))
    (df-add-series! int-data (make-series "y-var" #:data (build-vector 1000 (λ (x) (expt x 3)))))
    int-data))
(define fit-1
  (graph #:data fit-1-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "Not Taylor"
         (points)
         (fit #:degree 3 #:show-equation? #t)))

(define-runtime-path fit-2-data "./test-data/fit-2.dat")
(define fit-2
  (graph #:data points-5-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "No correlation"
         (points)
         (fit #:show-equation? #t)))

(define-runtime-path fit-3-data "./test-data/fit-3.dat")
(define fit-3
  (graph #:data points-1-df
         #:mapping (aes #:x "x-var" #:y "x-var")
         #:title "y=x"
         (points #:size 0)
         (fit #:show-equation? #t)))

(define-runtime-path fit-4-data "./test-data/fit-4.dat")
(define fit-4
  (graph #:data points-1-df
         #:mapping (aes #:x "x-var" #:y "x-var")
         #:title "y=x, but with no label"
         (points #:size 0)
         (fit)))

(define-runtime-path fit-5-data "./test-data/fit-5.dat")
(define fit-5-df
  (let ([int-data (make-data-frame)])
    (df-add-series! int-data (make-series "x-var" #:data (build-vector 1000 add1)))
    (df-add-series! int-data (make-series "y-var" #:data (build-vector 1000 (λ (x) (log (add1 x) 10)))))
    int-data))
(define fit-5
  (graph #:data fit-5-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "Fit is applied after transform"
         #:x-transform logarithmic-transform
         (points)
         (fit #:degree 3)))

(define-runtime-path fit-6-data "./test-data/fit-6.dat")
(define fit-6
  (graph #:data fit-5-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "If you don't transform..."
         (points)
         (fit #:degree 3)))

(define (random-in-list lst)
  (list-ref lst (random (length lst))))

(define-runtime-path bar-1-data "./test-data/bar-1.dat")
(define bar-1-df
  (begin
    (random-seed 4926)
    (let ([int-data (make-data-frame)])
      (df-add-series! int-data (make-series "whatever" #:data (build-vector 1000 (λ (_) (random)))))
      (df-add-series! int-data
                      (make-series "strat"
                                   #:data
                                   (build-vector 1000 (λ (_) (random-in-list '("a" "b" "c" "d" "q"))))))
      int-data)))
(define bar-1
  (graph #:data bar-1-df
         #:mapping (aes #:x "strat")
         #:title "Some random garbage"
         (bar)))

(module+ test
  (check-draw-steps points-1 points-1-data)
  (check-draw-steps points-2 points-2-data)
  (check-draw-steps points-3 points-3-data)
  (check-draw-steps points-4 points-4-data)
  (check-draw-steps points-5 points-5-data)

  (check-draw-steps lines-1 lines-1-data)
  (check-draw-steps lines-2 lines-2-data)

  (check-draw-steps fit-1 fit-1-data)
  (check-draw-steps fit-2 fit-2-data)
  (check-draw-steps fit-3 fit-3-data)
  (check-draw-steps fit-4 fit-4-data)
  (check-draw-steps fit-5 fit-5-data)
  (check-draw-steps fit-6 fit-6-data)

  (check-draw-steps bar-1 bar-1-data))

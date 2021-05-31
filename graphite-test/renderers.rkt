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

(define-runtime-path lines-3-data "./test-data/lines-3.dat")
(define lines-3-df
  (begin
    (random-seed 5010)
    (let ([int-data (make-data-frame)])
      (define-values (xs ys)
        (for/fold ([xs (list 0)] [ys (list 0)])
                  ([i (in-range 1 200)])
          (values (cons i xs) (cons (+ (first ys) (* 1/100 (- (random) 1/2))) ys))))
      (df-add-series! int-data (make-series "x-var" #:data (list->vector xs)))
      (df-add-series! int-data (make-series "y-var" #:data (list->vector ys)))
      int-data)))
(define lines-3
  (graph #:data lines-3-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         #:title "Random walk, 2"
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

(define-runtime-path error-bars-1-data "./test-data/error-bars-1.dat")
(define error-bars-1-df
  (let ()
    (define int-data (make-data-frame))
    (define (3x^2 x) (* 3.0 (expt x 2.0)))
    (define (add-error y) (+ y (* y (/ (- (random 4) 2) 10.0))))
    (df-add-series! int-data (make-series "x" #:data (build-vector 10 add1)))
    (df-add-series! int-data (make-series "3x^2" #:data (build-vector 10 (compose add-error 3x^2 add1))))
    (df-add-series! int-data (make-series "err" #:data (make-vector 10 0.2)))
    int-data))
(define error-bars-1
  (graph #:data error-bars-1-df
         #:mapping (aes #:x "x" #:y "3x^2")
         #:title "Error bars"
         (points)
         (fit #:degree 2)
         (error-bars #:mapping (aes #:perc-error "err"))))

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

(define-runtime-path bar-2-data "./test-data/bar-2.dat")
(define bar-2-df
  (let ([int-data (make-data-frame)])
    (df-add-series! int-data (make-series "x" #:data (vector-append (make-vector 1000 "a")
                                                                    (vector "b"))))
    int-data))
(define bar-2
  (graph #:data bar-2-df
         #:mapping (aes #:x "x")
         #:title "Overpowered"
         (bar)))

(define-runtime-path bar-3-data "./test-data/bar-3.dat")
(define bar-3-df
  (let ([int-data (make-data-frame)])
    (define-values (xs ys)
      (for/fold ([xs (vector)] [ys (vector)])
                ([i (in-range 10)])
        (values (vector-append xs (make-vector (sqr i) (string-ref "abcdefhijklm" i)))
                (vector-append ys (make-vector (sqr i) i)))))
    (df-add-series! int-data (make-series "x-var" #:data xs))
    (df-add-series! int-data (make-series "y-var" #:data ys)) 
    int-data))
(define bar-3
  (graph #:data bar-3-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         (bar #:label "big steppy")))

(define-runtime-path bar-4-data "./test-data/bar-4.dat")
(define bar-4
  (graph #:data bar-1-df
         #:mapping (aes #:x "strat")
         #:title "Some random garbage, proportionally(tm)"
         (bar #:mode 'prop)))

(define-runtime-path bar-5-data "./test-data/bar-5.dat")
(define bar-5
  (graph #:data bar-1-df
         #:mapping (aes #:x "strat")
         (bar #:mode 'prop #:mapping (aes #:group "strat"))))

(define-runtime-path bar-6-data "./test-data/bar-6.dat")
(define bar-6-df
  (begin
    (random-seed 1747)
    (let ([int-data (make-data-frame)])
      (define (blah-vector)
        (build-vector 1000 (λ (_) (random-in-list '("a" "b" "c" "d" "q")))))
      (df-add-series! int-data (make-series "x-var" #:data (blah-vector)))
      (df-add-series! int-data (make-series "group-var" #:data (blah-vector)))
      int-data)))
(define bar-6
  (graph #:data bar-6-df
         #:mapping (aes #:x "x-var")
         (bar #:mode 'prop #:mapping (aes #:group "group-var"))))

(define-runtime-path stacked-bar-1-data "./test-data/stacked-bar-1.dat")
(define stacked-bar-1
  (graph #:data bar-6-df
         #:mapping (aes #:x "x-var")
         (stacked-bar #:mapping (aes #:group "group-var"))))

(define-runtime-path histogram-1-data "./test-data/histogram-1.dat")
(define histogram-1
  (graph #:data points-5-df
         #:mapping (aes #:x "x-var")
         (histogram)))

(define-runtime-path histogram-2-data "./test-data/histogram-2.dat")
(define histogram-2
  (graph #:data points-5-df
         #:mapping (aes #:x "x-var" #:y "y-var")
         (histogram)))

(define-runtime-path density-1-data "./test-data/density-1.dat")
(define density-1
  (graph #:data points-5-df
         #:mapping (aes #:x "x-var")
         (density)))

(define-runtime-path density-2-data "./test-data/density-2.dat")
(define density-2
  (graph #:data points-5-df
         #:mapping (aes #:x "x-var")
         (density #:mapping (aes #:discrete-color "stratify-on"))))

(define-runtime-path boxplot-1-data "./test-data/boxplot-1.dat")
(define boxplot-1
  (graph #:data bar-1-df
         #:mapping (aes #:x "strat" #:y "whatever")
         (boxplot)))

(define-runtime-path boxplot-2-data "./test-data/boxplot-2.dat")
(define boxplot-2
  (graph #:data bar-1-df
         #:mapping (aes #:x "whatever" #:y "strat")
         (boxplot #:invert? #t)))

(define-runtime-path boxplot-3-data "./test-data/boxplot-3.dat")
(define boxplot-3
  (graph #:data bar-1-df
         #:mapping (aes #:x "strat" #:y "whatever")
         (boxplot #:iqr-scale 0.5)))

(define-runtime-path boxplot-4-data "./test-data/boxplot-4.dat")
(define boxplot-4
  (graph #:data bar-1-df
         #:mapping (aes #:x "strat" #:y "whatever")
         (boxplot #:iqr-scale 0.5 #:show-outliers? #f #:show-whiskers? #f)))

(module+ test
  (check-draw-steps points-1 points-1-data)
  (check-draw-steps points-2 points-2-data)
  (check-draw-steps points-3 points-3-data)
  (check-draw-steps points-4 points-4-data)
  (check-draw-steps points-5 points-5-data)

  (check-draw-steps lines-1 lines-1-data)
  (check-draw-steps lines-2 lines-2-data)
  (check-draw-steps lines-3 lines-3-data)

  (check-draw-steps fit-1 fit-1-data)
  (check-draw-steps fit-2 fit-2-data)
  (check-draw-steps fit-3 fit-3-data)
  (check-draw-steps fit-4 fit-4-data)
  (check-draw-steps fit-5 fit-5-data)
  (check-draw-steps fit-6 fit-6-data)

  (check-draw-steps error-bars-1 error-bars-1-data)

  (check-draw-steps bar-1 bar-1-data)
  (check-draw-steps bar-2 bar-2-data)
  (check-draw-steps bar-3 bar-3-data)
  (check-draw-steps bar-4 bar-4-data)
  (check-draw-steps bar-5 bar-5-data)
  (check-draw-steps bar-6 bar-6-data)

  (check-draw-steps stacked-bar-1 stacked-bar-1-data)

  (check-draw-steps histogram-1 histogram-1-data)
  (check-draw-steps histogram-2 histogram-2-data)

  (check-draw-steps density-1 density-1-data)
  (check-draw-steps density-2 density-2-data)

  (check-draw-steps boxplot-1 boxplot-1-data)
  (check-draw-steps boxplot-2 boxplot-2-data)
  (check-draw-steps boxplot-3 boxplot-3-data)
  (check-draw-steps boxplot-4 boxplot-4-data))

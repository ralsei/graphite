#lang racket
(require data-frame graphite rackunit
         racket/runtime-path
         "util.rkt")

(define-runtime-path facet-1-data "./test-data/facet-1.dat")
(define facet-1-df
  (begin
    (random-seed 254)
    (let ([int-data (make-data-frame)]
          [xs (build-vector 1000 (λ (_) (* (random) 30)))]
          [ys (build-vector 1000 (λ (_) (* (random) 30)))]
          [strats (for/vector ([_ (in-range 0 1000)]
                               [x (in-cycle (in-list '("a" "b" "c" "d" "e")))])
                    x)])
      (df-add-series! int-data (make-series "x-var" #:data xs))
      (df-add-series! int-data (make-series "y-var" #:data ys))
      (df-add-series! int-data (make-series "stratify-on" #:data strats))
      int-data)))
(define facet-1
  (graph #:data facet-1-df
         #:mapping (aes #:x "x-var" #:y "y-var" #:facet "stratify-on")
         (points)))

(define-runtime-path facet-2-data "./test-data/facet-2.dat")
(define facet-2
  (graph #:data facet-1-df
         #:mapping (aes #:x "x-var" #:y "y-var" #:facet "stratify-on")
         #:facet-wrap 5
         (lines)))

(define-runtime-path facet-3-data "./test-data/facet-3.dat")
(define facet-3
  (graph #:data facet-1-df
         #:mapping (aes #:x "x-var" #:facet "stratify-on")
         #:facet-wrap 1
         (density)))

(module+ test
  (check-draw-steps facet-1 facet-1-data)
  (check-draw-steps facet-2 facet-2-data)
  (check-draw-steps facet-3 facet-3-data))

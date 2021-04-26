#lang racket
(require fancy-app plot/utils)
(provide (contract-out [struct transform ((function invertible-function?)
                                          (axis-ticks ticks?))]
                       [only-ticks (-> ticks? transform?)]
                       [get-adjusted-ticks (-> transform? ticks?)])
         no-transform logarithmic-transform)

(struct transform (function axis-ticks) #:transparent)

(define (get-adjusted-ticks transform)
  (ticks-scale (transform-axis-ticks transform)
               (invertible-inverse (transform-function transform))))

(define no-transform
  (transform (invertible-function identity identity)
             (linear-ticks)))
(define logarithmic-transform
  (transform (invertible-function (log _ 10) (expt 10 _))
             (log-ticks #:scientific? #f)))

(define (only-ticks ticks)
  (transform (invertible-function identity identity) ticks))

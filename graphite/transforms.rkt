#lang racket
(require fancy-app plot/utils)
(provide (contract-out [struct transform ((function (-> any/c any/c))
                                          (inverse (-> any/c any/c))
                                          (axis-ticks ticks?))]
                       [only-ticks (-> ticks? transform?)]
                       [get-adjusted-ticks (-> transform? ticks?)])
         no-transform logarithmic-transform)

(struct transform (function inverse axis-ticks))

(define (get-adjusted-ticks transform)
  (ticks-scale (transform-axis-ticks transform)
               ; XXX: intentionally inverted
               (invertible-function (transform-inverse transform)
                                    (transform-function transform))))

(define no-transform
  (transform identity identity (linear-ticks)))
(define logarithmic-transform
  (transform (log _ 10) (expt 10 _) (log-ticks #:scientific? #f)))

(define (only-ticks ticks)
  (transform identity identity ticks))

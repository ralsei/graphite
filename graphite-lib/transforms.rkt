#lang racket/base
(require fancy-app
         plot/utils
         racket/contract/base
         racket/function)

(provide (contract-out [struct transform ((plot-transform axis-transform/c)
                                          (axis-ticks ticks?))]
                       [only-ticks (-> ticks? transform?)]
                       [get-adjusted-ticks (-> real? real? transform? ticks?)])
         no-transform logarithmic-transform get-conversion-function)

(struct transform (plot-transform axis-ticks))

(define (get-conversion-function axis-min axis-max conv transform)
  (cond [(and conv transform) (compose (invertible-function-f
                                        (apply-axis-transform (transform-plot-transform transform)
                                                              axis-min axis-max))
                                       conv)]
        [conv conv]
        [transform transform]
        [else identity]))

(define (get-adjusted-ticks axis-min axis-max transform)
  (ticks-scale (transform-axis-ticks transform)
               (invertible-inverse (apply-axis-transform
                                    (transform-plot-transform transform)
                                    axis-min axis-max))))

(define no-transform
  (transform id-transform (linear-ticks)))
(define logarithmic-transform
  (transform log-transform (log-ticks #:scientific? #f)))

(define (only-ticks ticks)
  (transform id-transform ticks))

#lang racket
(require data-frame graphite plot/utils)

(define df (make-data-frame))
(df-add-series! df (make-series "trt" #:data (vector "a" "b" "c")))
(df-add-series! df (make-series "outcome" #:data (vector 2.3 1.9 3.2)))

(parameterize ([plot-x-ticks no-ticks])
  (graph #:data df
         #:mapping (aes #:x "trt" #:y "outcome")
         (col)))

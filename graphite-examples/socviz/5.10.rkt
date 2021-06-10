#lang racket
(require data-frame graphite plot/utils)

(define organdata (df-read/csv "../data/organdata.csv" #:na "NA"))

(parameterize ([plot-y-ticks no-ticks]
               [plot-legend-layout '(rows 1 compact)])
  (graph #:data organdata
         #:mapping (aes #:x "donors" #:y "country")
         #:width 700
         #:y-min -1 #:y-max 17
         #:legend-anchor 'outside-top
         (points #:mapping (aes #:discrete-color "world"))))

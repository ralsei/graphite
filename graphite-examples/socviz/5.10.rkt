#lang racket
(require data-frame graphite plot/utils)

(random-seed 33) ; jittering

(define organdata (df-read/csv "../data/organdata.csv" #:na "NA"))

(parameterize ([plot-legend-layout '(rows 1 compact)]) ; TODO: make this part of theming
  (graph #:data organdata
         #:mapping (aes #:x "donors" #:y "country")
         #:width 700 #:height 500
         #:y-min -1 #:y-max 17 ; padding
         #:x-min 4 #:x-max 35
         #:legend-anchor 'outside-top
         (points #:mapping (aes #:discrete-color "world") #:y-jitter 0.15)))

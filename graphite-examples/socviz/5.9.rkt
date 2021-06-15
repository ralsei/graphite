#lang racket
(require data-frame graphite plot/utils)

(define organdata (df-read/csv "../data/organdata.csv" #:na "NA"))

(parameterize ([plot-x-label #f] [plot-y-label #f])
  (graph #:data organdata
         #:mapping (aes #:x "donors" #:y "country" #:facet "consent_law")
         #:width 700
         (boxplot #:invert? #t)))

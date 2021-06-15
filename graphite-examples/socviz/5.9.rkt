#lang racket
(require data-frame graphite)

(define organdata (df-read/csv "../data/organdata.csv" #:na "NA"))

(graph #:data organdata
       #:mapping (aes #:x "donors" #:y "country" #:facet "consent_law")
       #:width 700
       (boxplot #:invert? #t))

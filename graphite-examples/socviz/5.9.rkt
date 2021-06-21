#lang racket
(require data-frame graphite plot/utils)

(define organdata (df-read/csv "../data/organdata.csv" #:na "NA"))

(define p (graph #:data organdata
                 #:mapping (aes #:x "donors" #:y "country")
                 #:width 700
                 (boxplot #:invert? #t)))

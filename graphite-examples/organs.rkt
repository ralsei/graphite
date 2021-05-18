#lang racket
(require data-frame fancy-app graphite plot/utils)

(define organdata (df-read/csv "./data/organdata.csv" #:na "NA"))

(graph #:data organdata
       #:mapping (aes #:x "country" #:y "donors")
       (boxplot #:invert? #t))

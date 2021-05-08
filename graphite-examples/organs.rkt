#lang racket
(require data-frame fancy-app graphite plot/utils)

(define organdata (df-read/csv "./data/organdata.csv" #:na "NA"))

(graph #:data organdata
       #:mapping (aes #:x "country" #:y "donors")
       #:y-transform (only-ticks no-ticks)
       (boxplot #:mapping (aes #:invert? #t)))

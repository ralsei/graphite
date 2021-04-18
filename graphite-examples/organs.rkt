#lang racket
(require data-frame fancy-app graphite)

(define organdata (df-read/csv "./data/organdata.csv" #:na "NA"))

(module+ main
  (graph #:data organdata
         #:mapping (aes #:x "country" #:y "donors")
         (boxplot)))

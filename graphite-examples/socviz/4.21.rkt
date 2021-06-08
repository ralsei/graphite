#lang racket
(require data-frame graphite)

(define oecd (df-read/csv "../data/oecd.csv" #:na "NA"))

(graph #:data oecd
       #:mapping (aes #:x "year" #:y "diff")
       #:title "The US Life Expectancy Gap"
       #:x-label "Year" #:y-label "Difference in Years"
       #:y-min -2 #:y-max 2
       #:width 600 #:height 400
       #:legend-anchor 'no-legend
       (col #:mapping (aes #:discrete-color "hi_lo")))

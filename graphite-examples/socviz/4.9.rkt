#lang racket
(require data-frame graphite)

(define gss (df-read/csv "../data/gss_sm.csv"))

(graph #:data gss
       #:mapping (aes #:x "religion")
       #:width 800 #:height 600
       #:legend-anchor 'top-right
       (bar #:mapping (aes #:group "religion")))

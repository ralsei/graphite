#lang racket
(require data-frame graphite)

(define gss (df-read/csv "../data/gss_sm.csv"))

(graph #:data gss
       #:mapping (aes #:x "religion" #:facet "bigregion")
       #:width 700 #:height 700
       (bar #:mode 'prop))

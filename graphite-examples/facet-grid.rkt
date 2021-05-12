#lang racket/base
(require data-frame graphite)

(provide (all-defined-out))

(define all-data (df-read/csv "./data/gss_sm.csv"))

(graph #:data all-data
       #:title "Age vs. number of children among different genders and races"
       #:x-label "Age (yrs)"
       #:y-label "# of children"
       #:mapping (aes #:x "age" #:y "childs" #:facet "race")
       (points #:mapping (aes #:alpha 0.2))
       (fit #:mapping (aes #:width 3)))

#lang racket/base
(require data-frame graphite)

(provide (all-defined-out))

(define all-data (df-read/csv "./data/gss_sm.csv"))

(graph #:data all-data
       #:x-label "Age (yrs)"
       #:y-label "# of children"
       #:mapping (aes #:x "age" #:y "childs" #:facet "race")
       #:facet-wrap 3
       #:height 400
       #:width 800
       (points #:alpha 0.2))

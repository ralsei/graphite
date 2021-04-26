#lang racket
(require data-frame graphite)

(define all-data (df-read/csv "./data/gss_sm.csv"))

(graph #:data all-data
       #:title "Where people live, I guess?"
       #:x-label "Region"
       #:y-label "% of total"
       #:mapping (aes #:x "bigregion")
       (bar #:mode 'prop))

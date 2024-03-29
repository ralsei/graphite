#lang racket
(require data-frame fancy-app
         graphite)
(provide (all-defined-out))

(define all-data (df-read/csv "./data/gss_sm.csv"))

(module+ main
  (graph #:data all-data
        #:title "Where people live, I guess?"
        #:x-label "Region"
        #:y-label "% of total"
        #:mapping (aes #:x "bigregion")
        #:width 1200 #:height 600
        (stacked-bar #:mode 'count #:mapping (aes #:group "religion"))))

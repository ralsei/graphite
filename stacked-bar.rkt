#lang racket
(require data-frame fancy-app
         "lib/plot.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/gss_sm.csv"))

(module+ main
  (pplot #:data all-data
        #:title "Where people live, I guess?"
        #:x-label "Region"
        #:y-label "% of total"
        #:mapping (aes #:x "bigregion")
        #:width 1200 #:height 600
        (bar #:mode 'count)))

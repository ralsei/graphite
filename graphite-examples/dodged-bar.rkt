#lang racket
(require data-frame fancy-app
         graphite)
(provide (all-defined-out))

(define all-data (df-read/csv "./data/gss_sm.csv"))

(module+ main
  (define plt
    (pplot #:data all-data
           #:title "Where people live, I guess?"
           #:x-label "Region"
           #:y-label "% of total"
           #:mapping (aes #:x "bigregion")
           #:width 1200 #:height 600
           (bar #:mode 'prop #:mapping (aes #:group "religion" #:group-gap 2))))
  plt)

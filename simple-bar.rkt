#lang racket
(require data-frame fancy-app pict plot/pict "common.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/gss_sm.csv"))

(module+ main
  (define plt
    (pplot #:data all-data
           #:title "Where people live, I guess?"
           #:x-label "Region"
           #:y-label "% of total"
           #:mapping (aes #:x "bigregion")
           (bar #:mode 'prop)))
  (save-pict plt "/home/hazel/bar.png"))

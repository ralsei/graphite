#lang racket
(require data-frame fancy-app plot "common.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/gss_sm.csv"))

(module+ main
  (plot-new-window? #t)
  (pplot #:data all-data
         #:title "Religion, among, uh, some people"
         #:x-label "Religion"
         #:y-label "Count"
         #:mapping (aes #:x "relig")
         (bar)))

#lang racket
(require data-frame fancy-app plot "lib/plot.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/gss_sm.csv"))

(module+ main
  (plot-new-window? #t)
  (pplot #:data all-data
         #:title "Age vs. number of children among different genders and races"
         #:x-label "Age (yrs)"
         #:y-label "# of children"
         #:mapping (aes #:x "age" #:y "childs" #:facet-x "sex" #:facet-y "race")
         (ppoints #:mapping (aes #:alpha 0.2))
         (fit #:method 'power #:mapping (aes #:width 3))))

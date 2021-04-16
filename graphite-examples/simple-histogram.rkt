#lang racket/base
(require data-frame graphite plot/utils fancy-app)
(provide (all-defined-out))

(define midwest (df-read/csv "./data/midwest.csv"))

(module+ main
  (pplot #:data midwest
         #:mapping (aes #:x "area" #:y "popdensity" #:facet "state")
         #:y-transform (invertible-function (log _ 10) (expt 10 _))
         #:y-ticks (log-ticks #:scientific? #f)
         #:x-label "area"
         #:y-label "population density"
         (ppoints #:mapping (aes #:alpha 0.2))
         (fit #:method 'linear)))

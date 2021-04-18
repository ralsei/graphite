#lang racket/base
(require data-frame graphite plot/utils fancy-app)
(provide (all-defined-out))

(define midwest (df-read/csv "./data/midwest.csv"))

; XXX: this ain't a histogram anymore, bucko
(module+ main
  (graph #:data midwest
         #:mapping (aes #:x "area" #:y "popdensity" #:facet "state")
         #:y-transform (invertible-function (log _ 10) (expt 10 _))
         #:y-ticks (log-ticks #:scientific? #f)
         #:x-label "area"
         #:y-label "population density"
         (points #:mapping (aes #:alpha 0.2))
         (fit #:method 'linear)))

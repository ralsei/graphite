#lang racket/base
(require data-frame graphite plot/utils fancy-app)
(provide (all-defined-out))

(define midwest (df-read/csv "./data/midwest.csv"))

(graph #:data midwest
       #:mapping (aes #:x "popdensity" #:y "percblack")
       #:x-label "population density"
       #:y-label "% black"
       (histogram #:bins 30))

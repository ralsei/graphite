#lang racket/base
(require data-frame graphite plot/utils fancy-app)
(provide (all-defined-out))

(define midwest (df-read/csv "./data/midwest.csv"))

(module+ main
  (graph #:data midwest
         #:mapping (aes #:x "area" #:facet "state")
         #:x-label "area"
         #:y-label "count"
         #:y-max 30
         (histogram #:bins 20)))

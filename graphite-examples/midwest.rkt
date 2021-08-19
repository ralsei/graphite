#lang racket
(require data-frame graphite)

(define midwest (df-read/csv "./data/midwest.csv"))

(graph #:data midwest
       #:x-label "area"
       #:y-label "density"
       #:mapping (aes #:x "area")
       #:width 640
       #:height 480
       (density #:mapping (aes #:discrete-color "state")))

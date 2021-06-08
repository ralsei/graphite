#lang racket
(require data-frame graphite)

(define midwest (df-read/csv "../data/midwest.csv"))

(graph #:data midwest
       #:mapping (aes #:x "area")
       #:width 700 #:height 500
       (density #:mapping (aes #:discrete-color "state")))

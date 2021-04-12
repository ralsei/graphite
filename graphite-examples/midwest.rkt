#lang racket
(require data-frame graphite)

(define midwest (df-read/csv "./data/midwest.csv"))

(module+ main
  (pplot #:data midwest
         #:title "I suck at titles?"
         #:x-label "area"
         #:y-label "density"
         #:mapping (aes #:x "area")
         #:width 640
         #:height 480
         (pdensity #:mapping (aes #:discrete-color "state"))))

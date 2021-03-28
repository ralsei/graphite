#lang racket
(require data-frame "lib/plot.rkt")

(define midwest (df-read/csv "./data/midwest.csv"))

(module+ main
  (pplot #:data midwest
         #:mapping (aes #:x "area")
         #:width 640 #:height 480
         (histogram #:bins 30)))

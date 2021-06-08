#lang racket
(require data-frame gregor plot/utils graphite)
(provide (all-defined-out))

(define chic-raw (df-read/csv "data/chicago-nmmaps.csv"))

(graph #:data chic-raw
       #:title "Temperatures in Chicago"
       #:x-label "Year"
       #:y-label "Temperature (degrees F)"
       #:mapping (aes #:x "date" #:y "temp")
       #:x-transform (only-ticks (date-ticks))
       #:x-conv (compose ->posix iso8601->date)
       #:width 600
       (lines))

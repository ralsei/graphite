#lang racket
(require data-frame gregor plot/utils graphite)
(provide (all-defined-out))

(define chic-raw (df-read/csv "data/chicago-nmmaps.csv"))

(define x-conv (compose ->posix iso8601->date))

(graph #:data chic-raw
       #:title "Temperatures in Chicago"
       #:x-label "Year"
       #:y-label "Temperature (degrees F)"
       #:mapping (aes #:x "date" #:y "temp")
       #:x-transform (transform (stretch-transform (x-conv "1998-01-01")
                                                   (x-conv "1999-01-01")
                                                   10)
                                (date-ticks))
       #:x-conv x-conv
       #:width 600
       (points))

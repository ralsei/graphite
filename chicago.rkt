#lang racket
(require data-frame gregor net/http-easy plot threading
         "common.rkt")
(provide (all-defined-out))

(define chic-url "https://raw.githubusercontent.com/Z3tt/R-Tutorials/master/ggplot2/chicago-nmmaps.csv")
(define chic-raw
  (~> (get #:stream? #t chic-url)
      response-output
      df-read/csv))

(module+ main
  (plot-new-window? #t)
  (pplot #:data chic-raw
         #:mapping (hash 'x "date" 'y "temp"
                         'title "Temperatures in Chicago"
                         'x-label "Year"
                         'y-label "Temperature (degrees F)")
         #:x-ticks (date-ticks)
         #:x-conv (compose ->posix iso8601->date)
         (ppoints)))

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
  (plot-new-window? #t) ; #f for drracket

  (pplot #:data chic-raw
         #:mapping (hash 'x "date" 'y "temp")
         #:x-ticks (date-ticks)
         #:x-conv (compose ->posix iso8601->date)
         (ppoints))

  #;
  (parameterize ([plot-title "Temperatures in Chicago"]
                 [plot-x-label "Year"]
                 [plot-y-label "Temperature (degrees F)"]
                 [plot-font-face "Arial"]
                 [point-color 'firebrick]
                 [point-sym 'bullet]
                 [plot-x-ticks (date-ticks)])
    (plot (dataframe chic-raw
                     "date" #:x-conv (compose ->posix iso8601->date)
                     "temp"))))

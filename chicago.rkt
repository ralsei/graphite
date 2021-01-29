#lang racket
(require data-frame gregor net/url plot)
(provide (all-defined-out))

(define chic-url "https://raw.githubusercontent.com/Z3tt/R-Tutorials/master/ggplot2/chicago-nmmaps.csv")
(define chic-raw ((compose df-read/csv get-pure-port string->url) chic-url))

; generate-plot : -> plot
(define (generate-plot)
  (parameterize ([plot-title "Temperatures in Chicago"]
                 [plot-x-label "Year"]
                 [plot-y-label "Temperature (degrees F)"]
                 [plot-font-face "Arial"]
                 [point-color 'firebrick]
                 [point-sym 'bullet]
                 [plot-x-ticks (date-ticks)])
    (plot (points
           (vector-map vector
            (vector-map (compose ->posix iso8601->date)
                        (df-select chic-raw "date"))
            (df-select chic-raw "temp"))))))

(module+ main
  (plot-new-window? #t) ; #f for drracket
  (generate-plot))

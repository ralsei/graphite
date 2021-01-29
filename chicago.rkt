#lang racket
(require data-frame gregor net/url plot)
(provide (all-defined-out))

(define chic-url "https://raw.githubusercontent.com/Z3tt/R-Tutorials/master/ggplot2/chicago-nmmaps.csv")
(define chic-raw ((compose df-read/csv get-pure-port string->url) chic-url))

; df-plot : [A B C D] dataframe string string (A -> B) (C -> D) -> plot
; given a dataframe, the data to use for the x-axis, the data to use
; for the y-axis, and conversion functions for both, plot it
(define (df-plot df x-axis y-axis x-conv y-conv)
  (define x-axis* (string-append x-axis "*"))
  (define y-axis* (string-append y-axis "*"))

  (df-add-derived df x-axis* `(,x-axis) (compose x-conv first))
  (df-add-derived df y-axis* `(,y-axis) (compose y-conv first))
  (plot (points (df-select* df x-axis* y-axis*))))

(module+ main
  (plot-new-window? #t) ; #f for drracket
  (parameterize ([plot-title "Temperatures in Chicago"]
                 [plot-x-label "Year"]
                 [plot-y-label "Temperature (degrees F)"]
                 [plot-font-face "Arial"]
                 [point-color 'firebrick]
                 [point-sym 'bullet]
                 [plot-x-ticks (date-ticks)])
    (df-plot chic-raw
             "date" "temp"
             (compose ->posix iso8601->date) identity)))

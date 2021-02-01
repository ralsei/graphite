#lang racket
(require data-frame gregor net/http-easy plot threading)
(provide (all-defined-out))

(define chic-url "https://raw.githubusercontent.com/Z3tt/R-Tutorials/master/ggplot2/chicago-nmmaps.csv")
(define chic-raw
  (~> (get #:stream? #t chic-url)
      response-output
      df-read/csv))

; df-plot : [A B C D] dataframe string string (A -> B) (C -> D) -> plot
; given a dataframe, the data to use for the x-axis, the data to use
; for the y-axis, and conversion functions for both, plot it
(define (df-plot df x-axis y-axis #:x-conv [x-conv values] #:y-conv [y-conv values])
  (plot (points
         (for/vector ([(x y) (in-data-frame df x-axis y-axis)])
           (vector (x-conv x) (y-conv y))))))

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
             "date" #:x-conv (compose ->posix iso8601->date)
             "temp")))

#lang racket
(require csv-reading gregor net/url plot)

(define chic-url "https://raw.githubusercontent.com/Z3tt/R-Tutorials/master/ggplot2/chicago-nmmaps.csv")
(define chic-raw ((compose csv->list get-pure-port string->url) chic-url))

; get-column : (listof (list A)) string -> (list A)
(define (get-column csv key)
  (define idx (index-of (first csv) key))
  (for/list ([row (in-list (rest csv))])
    (list-ref row idx)))

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
           (map vector
                (map (compose ->posix iso8601->date)
                     (get-column chic-raw "date"))
                (map string->number (get-column chic-raw "temp")))))))

(module+ main
  (plot-new-window? #t) ; #f for drracket
  (generate-plot))

#lang racket
(require data-frame fancy-app plot/pict
         graphite)
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(module+ main
  (pplot #:data all-data
         #:mapping (aes #:x "year" #:y "gdpPercap"
                        #:discrete-color "country" #:facet "continent")
         #:x-label "Year" #:y-label "GDP per capita (USD)"
         #:y-transform (invertible-function (log _ 10) (expt 10 _))
         #:y-ticks (log-ticks #:scientific? #f)
         #:y-max 5.1
         #:width 400 #:height 400
         #:legend-anchor 'no-legend
         (plines #:mapping (aes #:color "gray"))
         (fit #:method 'linear #:mapping (aes #:width 2))))

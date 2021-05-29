#lang racket
(require data-frame fancy-app plot/utils
         graphite)
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(graph #:data all-data
       #:mapping (aes #:x "year" #:y "gdpPercap"
                      #:discrete-color "country" #:facet "continent")
       #:x-label "Year" #:y-label "GDP per capita (USD)"
       #:y-transform logarithmic-transform
       #:width 1200 #:height 500
       #:legend-anchor 'no-legend
       (lines #:color "gray")
       (fit #:width 2))

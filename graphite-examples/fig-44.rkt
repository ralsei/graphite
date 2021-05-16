#lang racket
(require data-frame fancy-app plot/utils
         graphite)
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(graph #:data all-data
       #:mapping (aes #:x "year" #:y "gdpPercap"
                      #:discrete-color "country" #:facet "continent"
                      #:facet-wrap 3)
       #:x-label "Year" #:y-label "GDP per capita (USD)"
       #:y-transform logarithmic-transform
       #:y-max 5.1
       #:width 400 #:height 400
       #:legend-anchor 'no-legend
       (lines #:mapping (aes #:color "gray"))
       (fit #:mapping (aes #:width 2)))

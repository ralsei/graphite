#lang racket
(require data-frame graphite)

(define gapminder (df-read/csv "../data/all_gapminder.csv"))

(graph #:data gapminder
       #:mapping (aes #:x "year" #:y "gdpPercap"
                      #:discrete-color "country" #:facet "continent")
       #:facet-wrap 2 ; maybe this should be called facet-ncol
       #:x-label "Year" #:y-label "GDP per capita"
       #:y-transform logarithmic-transform
       #:width 600 #:height 700
       #:legend-anchor 'no-legend
       (lines #:color "gray")
       (fit #:method 'loess #:width 2))

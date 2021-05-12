#lang racket
(require data-frame fancy-app graphite)

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(graph #:data all-data
       #:title "GDP per capita vs life expectancy"
       #:x-label "GDP per capita (USD)"
       #:y-label "Life expectancy (years)"
       #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:discrete-color "continent")
       #:x-transform logarithmic-transform
       (points #:mapping (aes #:alpha 0.4))
       (fit #:mapping (aes #:width 3)))

#lang racket
(require data-frame fancy-app graphite)
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(module+ main
  (graph #:data all-data
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:discrete-color "continent")
         #:x-transform logarithmic-transform
         (points #:mapping (aes #:alpha 0.4))
         (fit #:method 'linear #:mapping (aes #:width 3))))

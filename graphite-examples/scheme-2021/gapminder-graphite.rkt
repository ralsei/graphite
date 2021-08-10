#lang racket
(require data-frame graphite)
(provide gapminder-graphite)

(define gapminder
  (df-read/csv "../data/all_gapminder.csv"))

(define gapminder-graphite
  (graph #:data gapminder
         #:mapping (aes #:x "gdpPercap"
                        #:y "lifeExp")
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:x-transform logarithmic-transform
         (points #:alpha 0.4
                 #:mapping (aes #:discrete-color
                                "continent"))
         (fit #:width 3 #:method 'loess)))

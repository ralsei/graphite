#lang racket
(require data-frame graphite)

(define gapminder (df-read/csv "../data/all_gapminder.csv"))

(graph #:data gapminder
       #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
       #:x-transform logarithmic-transform
       #:width 1000 #:height 600
       (points #:mapping (aes #:discrete-color "continent"))
       (fit #:method 'loess #:width 3))
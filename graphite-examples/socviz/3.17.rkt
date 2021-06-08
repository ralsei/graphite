#lang racket
(require data-frame graphite)

(define gapminder (df-read/csv "../data/all_gapminder.csv"))
(df-add-derived! gapminder "logPop" '("pop")
                 (λ (x) (log (first x) 10)))

(graph #:data gapminder
       #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
       #:x-transform logarithmic-transform
       #:width 1000 #:height 600
       #:theme theme-continuous ; I don't like this color-map, tbh --
       (points #:mapping (aes #:continuous-color "logPop")))

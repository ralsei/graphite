#lang racket
(require data-frame fancy-app plot/pict
         graphite)
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(module+ main
  (pplot #:data all-data
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:discrete-color "continent")
         #:x-transform (invertible-function (log _ 10) (expt 10 _))
         #:x-ticks (log-ticks #:scientific? #f)
         (ppoints #:mapping (aes #:alpha 0.4))
         (fit #:method 'linear #:mapping (aes #:width 3))))

#lang racket
(require data-frame fancy-app plot/pict "common.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(module+ main
  (pplot #:data all-data
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:discrete-color "continent")
         ;#:x-transform log-transform
         #:x-conv (log _ 10)
         #:x-ticks (ticks-scale (log-ticks)
                                (invertible-function (expt 10 _) (log _ 10)))
         (ppoints #:mapping (aes #:alpha 0.4))
         (fit #:method 'linear #:mapping (aes #:width 3))))

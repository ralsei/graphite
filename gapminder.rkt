#lang racket
(require data-frame fancy-app plot/pict "common.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(module+ main
  (define pct
    (pplot #:data all-data
           #:title "GDP per capita vs life expectancy"
           #:x-label "GDP per capita (USD)"
           #:y-label "Life expectancy (years)"
           #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:discrete-color "continent")
           #:x-conv (log _ 10)
           #:x-ticks (ticks-scale (log-ticks #:scientific? #f)
                                  (invertible-function (expt 10 _) (log _ 10)))
           (ppoints #:mapping (aes #:alpha 0.4))
           (fit #:method 'linear #:mapping (aes #:width 3))))
  (save-pict pct "/home/hazel/gapminder.png"))

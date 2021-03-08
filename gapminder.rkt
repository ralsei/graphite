#lang racket
(require data-frame fancy-app plot/pict
         "lib/plot.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(module+ main
  (define pct
    (pplot #:data all-data
           #:title "GDP per capita vs life expectancy"
           #:x-label "GDP per capita (USD)"
           #:y-label "Life expectancy (years)"
           #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:discrete-color "continent")
           #:x-conv (invertible-function (log _ 10) (expt 10 _))
           #:x-conv-ticks? #t
           #:x-ticks (log-ticks #:scientific? #f)
           (ppoints #:mapping (aes #:alpha 0.4))
           (fit #:method 'linear #:mapping (aes #:width 3))))
  (save-pict pct "./gapminder.png"))

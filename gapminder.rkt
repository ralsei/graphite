#lang racket
(require data-frame plot "common.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(module+ main
  (plot-new-window? #t)
  (pplot #:data all-data
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:discrete-color "continent"
                        #:title "GDP per capita vs life expectancy"
                        #:x-label "GDP per capita (USD)"
                        #:y-label "Life expectancy (years)")
         #:x-transform log-transform
         #:x-ticks (currency-ticks)
         (ppoints)
         (fit #:method 'polynomial #:poly-degree 2 #:mapping (aes #:width 3))))

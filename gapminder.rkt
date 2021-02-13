#lang racket
(require data-frame plot "common.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/gapminder.csv"))

(module+ main
  (plot-new-window? #t)
  (pplot #:data all-data
         #:mapping (hash 'x "gdpPerCapita" 'y "lifeExpectancy" 'discrete-color "continent"
                         'title "GDP per capita vs life expectancy from 2000-2019"
                         'x-label "GDP per capita (USD)"
                         'y-label "Life expectancy (years)")
         #:x-transform log-transform
         #:x-ticks (currency-ticks)
         (ppoints)
         (fit #:method 'polynomial #:poly-degree 3 #:mapping (hash 'width 3))))

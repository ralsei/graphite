#lang racket
(require data-frame fancy-app plot "common.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/all_gapminder.csv"))

(module+ main
  (plot-new-window? #t)
  (pplot #:data all-data
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:discrete-color "continent")
         #:x-conv (log _ 10)
         #:x-ticks (log-ticks)
         (ppoints)
         (fit #:method 'linear #:mapping (aes #:width 3))))

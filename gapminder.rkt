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
         #:x-transform log-transform
         ;#:x-conv (log _ 10)
         ;#:x-ticks (ticks-scale (linear-ticks) (invertible-function (expt _ 10) (log _ 10)))
         (ppoints)
         (fit #:method 'power #:mapping (aes #:width 3))))

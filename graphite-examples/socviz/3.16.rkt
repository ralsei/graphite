#lang racket
(require data-frame graphite graphite/parameters graphite/titles plot/utils)

(define gapminder (df-read/csv "../data/all_gapminder.csv"))

(define v (graph #:data gapminder
                 #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
                 #:x-transform logarithmic-transform
                 ;#:width 500 #:height 500
                 (points #:mapping (aes #:discrete-color "continent"))
                 (fit #:method 'loess #:width 3)))

(parameterize ([gr-title "title blahblahblah it's gapminder again"]
               [gr-x-label "WE LIVE IN A SOCIETY"]
               [gr-y-label "BOTTOM TEXT"]
               [gr-facet-label "facet label (centered on plot, not x-axis)"]
               [plot-font-size 11]
               [plot-font-family 'swiss])
  (add-all-titles (add-facet-label v)))

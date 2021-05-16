#lang racket
(require colormaps data-frame graphite plot/utils)

(define gapminder (df-read/csv "data/all_gapminder.csv"))
(df-add-derived! gapminder "log-pop" (list "pop")
                 (λ (x) (log (first x) 10)))

(parameterize ([plot-pen-color-map 'tol-ys])
  (graph #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:continuous-color "log-pop")
         #:x-transform logarithmic-transform
         #:legend-anchor 'no-legend
         (points)))

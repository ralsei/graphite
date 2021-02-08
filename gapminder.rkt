#lang racket
(require data-frame plot
         "common.rkt")
(provide (all-defined-out))

(define all-data (df-read/csv "./data/gapminder.csv"))

; transpose : [A] [vectorof [vectorof A]] -> [vectorof [vectorof A]]
; transposes a 2d vector as if it was a matrix
(define (transpose vecvec)
  (for/vector ([i (in-range (vector-length (vector-ref vecvec 0)))])
    (for/vector ([il (in-vector vecvec)])
      (vector-ref il i))))

(define (df-filter df pred?)
  (define series (df-series-names df))
  (define rows
    (apply (curry df-select* df #:filter pred?) series))
  (define full (vector-append (vector (list->vector series)) rows))

  (define ret (make-data-frame))
  (for* ([row (in-vector (transpose full))])
    (match-define-values ((vector name) data) (vector-split-at row 1))
    (df-add-series ret (make-series name #:data data)))
  ret)

(module+ main
  (define fit
    (df-least-squares-fit all-data "gdpPerCapita" "lifeExpectancy"
                          #:mode 'polynomial #:polynomial-degree 8))

  (plot-new-window? #t)
  (parameterize ([plot-title "GDP per capita vs life expectancy from 2000-2019"]
                 [plot-x-label "GDP per capita (USD)"]
                 [plot-y-label "Life expectancy (years)"]
                 [plot-font-face "Arial"]
                 [plot-x-transform log-transform]
                 [plot-x-ticks (currency-ticks)]
                 [point-sym 'bullet]
                 [point-alpha 0.3])
   (plot
     ; this could be abstracted
     (list (dataframe (df-filter all-data (curry vector-member "Africa"))
                      "gdpPerCapita" "lifeExpectancy"
                      #:color 'red #:label "Africa")
           (dataframe (df-filter all-data (curry vector-member "Asia"))
                      "gdpPerCapita"
                      "lifeExpectancy"
                      #:color 'orange #:label "Asia")
           (dataframe (df-filter all-data (curry vector-member "Europe"))
                      "gdpPerCapita" "lifeExpectancy"
                      #:color 'green #:label "Europe")
           (dataframe (df-filter all-data (curry vector-member "North America"))
                      "gdpPerCapita" "lifeExpectancy"
                      #:color 'blue #:label "North America")
           (dataframe (df-filter all-data (curry vector-member "Oceania"))
                      "gdpPerCapita" "lifeExpectancy"
                      #:color 'navy #:label "Oceania")
           (dataframe (df-filter all-data (curry vector-member "South America"))
                      "gdpPerCapita" "lifeExpectancy"
                      #:color 'magenta #:label "South America")
           (function fit
                     #:width 3
                     #:color 'blue)))))

#lang racket
(require data-frame plot fancy-app
         "common.rkt" plot/utils)
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
                          #:mode 'polynomial #:polynomial-degree 3))

  (plot-new-window? #t)
  (parameterize ([plot-title "GDP per capita vs life expectancy from 2000-2019"]
                 [plot-x-label "GDP per capita (USD)"]
                 [plot-y-label "Life expectancy (years)"]
                 [plot-font-face "Arial"]
                 [plot-x-transform log-transform]
                 [plot-x-ticks (currency-ticks)]
                 [point-sym 'bullet]
                 [plot-pen-color-map 'set1])
    #;#;
    (define a (aes all-data #:x "gdpPerCapita" #:y "lifeExpectancy" #:discrete-color "continent"))
    (plot (list (points a)
                (function-fit a 'least-squares)))

    #;
    (plot #:data all-data
          #:mapping (hash 'x "gdpPerCapita" 'y "lifeExpectancy" 'discrete-color "continent")
          #:x-transform log-transform
          #:x-ticks (currency-ticks)
          (points)
          (fit #:method 'loess))
    (define tbl (make-hash))
    (for ([(x y con) (in-data-frame all-data "gdpPerCapita" "lifeExpectancy" "continent")]
          #:when (and x y))
      (hash-update! tbl con (cons (vector x y) _) null))

    (plot
     (cons (function fit #:width 3 #:color 'blue)
           (let ([color-n -1])
           (hash-map tbl
                     (lambda (con pts)
                       (set! color-n (add1 color-n))
                       (points pts #:color (->pen-color color-n) #:label con))
                     #t))))
    #;
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

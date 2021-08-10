#lang racket
(require data-frame
         fancy-app
         plot/pict
         plot/utils)
(provide gapminder-plot)

(define gapminder (df-read/csv "../data/all_gapminder.csv"))

(define fit
  (df-least-squares-fit gapminder "gdpPercap"
                        "lifeExp" #:mode 'log))

(define gapminder-plot
  (parameterize ([plot-x-label "GDP per capita (USD)"]
                 [plot-y-label "Life expectancy (years)"]
                 [plot-font-family 'swiss]
                 [plot-x-transform log-transform]
                 [plot-x-ticks (log-ticks #:scientific? #f)]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-far-ticks no-ticks]
                 [point-sym 'bullet]
                 [point-alpha 0.4]
                 [plot-pen-color-map 'set1])
    (define tbl (make-hash))
    (for ([(x y con) (in-data-frame gapminder
                                    "gdpPercap"
                                    "lifeExp"
                                    "continent")]
          #:when (and x y))
      (hash-update! tbl con (cons (vector x y) _) null))

    (plot
     (cons (function fit #:width 3 #:color 'blue)
           (let ([color-n -1])
             (hash-map tbl
                       (lambda (con pts)
                         (set! color-n (add1 color-n))
                         (points pts
                                 #:color (->pen-color color-n)
                                 #:label con))))))))

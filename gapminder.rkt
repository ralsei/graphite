#lang racket
(require data-frame gregor plot
         "common.rkt")
(provide (all-defined-out))

(define gdp-messy (df-read/csv "./data/gdppercapita_us_inflation_adjusted.csv"))
(define life-messy (df-read/csv "./data/life_expectancy_years.csv"))
(define continents (df-read/csv "./data/Countries-Continents.csv"))

; gross, and very specific. frankly, there should be an entire library for this.
; I also don't like how I have to use mutability for this, since there's no constructor for
; a data-frame with existing data in it aside from reading from a file.
(define (tidify df start stop observation-name)
  (define countries (df-select df "country"))

  (define rows
    (for*/vector ([year (in-range start stop)]
                  [country (in-indexed (in-vector countries))])
      ; temporary hack from hell
      (define idx (index-of (vector->list countries) country))
      (vector country year (vector-ref (df-select df (~a year)) idx))))

  (define country-series (make-series "country" #:data (vector-map (λ (v) (vector-ref v 0)) rows)))
  (define year-series (make-series "year" #:data (vector-map (λ (v) (vector-ref v 1)) rows)))
  (define data-series (make-series observation-name #:data (vector-map (λ (v) (vector-ref v 2)) rows)))

  ; why???
  (define ret (make-data-frame))
  (df-add-series ret country-series)
  (df-add-series ret year-series)
  (df-add-series ret data-series)
  ret)

(define gdp (tidify gdp-messy 2000 2020 "gdp-per-capita"))
(define life (tidify life-messy 2000 2020 "life-expectancy"))

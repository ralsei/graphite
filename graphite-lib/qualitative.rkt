#lang racket/base
(require data-frame
         fancy-app
         plot/utils
         racket/match
         racket/vector
         "ordering.rkt"
         "util.rkt")
(provide qualitative? qualitative-iso qualitative-ticks)

; determines if a variable in the data-frame is qualitative
(define (qualitative? var)
  (not (real? (vector-ref (df-select (gr-data) var) 0))))

; creates an isomorphism between the variable and the reals
(define (qualitative-iso var+ord)
  (match-define (variable+ordering var-name ord) var+ord)
  (define cmp
    (let ([initial-fn (ordering-func ord)])
      (if (dependent-ordering? ord)
          (initial-fn var-name) ; we should have parameterized gr-data by now
          initial-fn)))
  (define vs (vector-sort (possibilities (gr-data) var-name) cmp))
  (define hsh
    (for/hash ([(v idx) (in-indexed (in-vector vs))])
      (values v idx)))
  (values vs
          (λ (name) (hash-ref hsh name))
          (λ (idx) (vector-ref vs idx))))

; generates ticks for the given axis and qualitative variable
(define (qualitative-ticks var+ord tick-fn)
  (define-values (sorted var->real _) (qualitative-iso var+ord))
  (tick-fn
   (for/list ([v (in-vector sorted)])
     (tick (var->real v) #t v))))

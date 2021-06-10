#lang racket/base
(require data-frame
         fancy-app
         plot/utils
         racket/match
         racket/vector
         "util.rkt")
(provide qualitative? qualitative-iso qualitative-ticks)

; determines if a variable in the data-frame is qualitative
(define (qualitative? var)
  (for/first ([v (in-vector (df-select (gr-data) var))]
              #:when v)
    (not (real? v))))

; creates an isomorphism between the variable and the reals
(define (qualitative-iso var-name)
  (define vs (possibilities (gr-data) var-name))
  (define hsh
    (for/hash ([(v idx) (in-indexed (in-vector vs))])
      (values v idx)))
  (values vs
          (λ (name) (hash-ref hsh name))
          (λ (idx) (vector-ref vs idx))))

; generates ticks for the given axis and qualitative variable
(define (qualitative-ticks var-name tick-fn)
  (define-values (vs var->real _) (qualitative-iso var-name))
  (tick-fn
   (for/list ([v (in-vector vs)])
     (tick (var->real v) #t v))))

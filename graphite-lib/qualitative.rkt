#lang racket/base
(require data-frame
         fancy-app
         plot/utils
         "util.rkt")
(provide qualitative? qualitative-iso qualitative-ticks variable-iso)

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
(define (qualitative-ticks var-name tick-fn
                           #:start-at [start-at 0] #:skip-by [skip-by 1])
  (define-values (vs var->real _) (qualitative-iso var-name))
  (tick-fn
   (for/list ([v (in-vector vs)])
     (tick (+ start-at (* skip-by (var->real v))) #t v))))

; creates an isomorphism between the variable and the reals, even if the variable is not
; qualitative
(define (variable-iso var-name)
  (cond [(qualitative? var-name) (qualitative-iso var-name)]
        [else (values (vector) values values)]))

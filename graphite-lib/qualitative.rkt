#lang racket/base
(require data-frame
         fancy-app
         plot/utils
         racket/match
         "util.rkt")
(provide qualitative? qualitative-iso qualitative-ticks variable-iso)

(define (conversion-function var)
  (match var
    ['x (gr-x-conv)]
    ['y (gr-y-conv)]
    [_ values]))

; determines if a variable in the data-frame is qualitative
(define (qualitative? mapping var)
  (for/first ([v (in-vector (df-select (gr-data) (hash-ref mapping var)))]
              #:when v)
    (not (real? ((conversion-function var) v)))))

; creates an isomorphism between the variable and the reals
(define (qualitative-iso mapping var)
  (define vs (possibilities (gr-data) (hash-ref mapping var)))
  (define hsh
    (for/hash ([(v idx) (in-indexed (in-vector vs))])
      (values ((conversion-function var) v) idx)))
  (values vs
          (λ (name) (hash-ref hsh name))
          (λ (idx) (vector-ref vs idx))))

; generates ticks for the given axis and qualitative variable
(define (qualitative-ticks mapping var tick-fn
                           #:start-at [start-at 0] #:skip-by [skip-by 1])
  (define (do-ticks)
    (define-values (vs var->real _) (qualitative-iso mapping var))
    (tick-fn
      (for/list ([v (in-vector vs)])
        (tick (+ start-at (* skip-by (var->real v))) #t v))))
  (define (do-ticks-or v)
    (if v (do-ticks) no-renderer))
  (match var
    ['x (do-ticks-or (gr-add-x-ticks?))]
    ['y (do-ticks-or (gr-add-y-ticks?))]
    [_ (do-ticks)]))

; creates an isomorphism between the variable and the reals, even if the variable is not
; qualitative
(define (variable-iso mapping var)
  (cond [(qualitative? mapping var) (qualitative-iso mapping var)]
        [else (values (possibilities (gr-data) (hash-ref mapping var))
                      values values)]))

#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide gr-data gr-global-mapping gr-x-conv gr-y-conv gr-group
         gr-x-min gr-x-max gr-y-min gr-y-max
         gr-bars-add-ticks?)

(define-syntax (define-parameter stx)
  (syntax-parse stx
    [(_ NAME VALUE) #'(define NAME (make-parameter VALUE))]
    [(_ NAME) #'(define NAME (make-parameter #f))]
    [_ (raise-syntax-error 'define-parameter
                           (format "expected a name and a value, or just a name"))]))

(define-parameter gr-data)
(define-parameter gr-global-mapping)
(define-parameter gr-x-conv (λ (x) x))
(define-parameter gr-y-conv (λ (x) x))
(define-parameter gr-group)

(define-parameter gr-x-min)
(define-parameter gr-x-max)
(define-parameter gr-y-min)
(define-parameter gr-y-max)

(define-parameter gr-bars-add-ticks? #t)

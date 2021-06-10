#lang racket/base
(require data-frame
         fancy-app
         math/statistics
         racket/contract
         "parameters.rkt")
(provide
 (contract-out
  [make-ordering (->i ([fn (dependent?)
                         (if (not dependent?)
                             (-> string? (-> any/c any/c boolean?))
                             (-> any/c any/c boolean?))])
                      (#:dependent? [dependent? boolean?])
                      [_ (dependent?)
                        (if (not dependent?)
                            independent-ordering?
                            dependent-ordering?)])]
  [ordering-func (->i ([ord (or/c independent-ordering? dependent-ordering?)])
                      [_ (ord)
                        (if (independent-ordering? ord)
                            (-> any/c any/c boolean?)
                            (-> string? (-> any/c any/c boolean?)))])]
  [ordering? (-> any/c boolean?)]
  [independent-ordering? (-> any/c boolean?)]
  [dependent-ordering? (-> any/c boolean?)]

  [raw-variable? (-> any/c boolean?)]
  [struct variable+ordering ((name string?) (order ordering?))]
  [variable? (-> any/c boolean?)]
  [variable-name (-> variable? string?)]

  [lexicographic independent-ordering?]
  [lexicographic-ci independent-ordering?]
  [mean-of (-> raw-variable? dependent-ordering?)]))

(struct ordering (func))
(struct independent-ordering ordering ())
(struct dependent-ordering ordering ())

(define (make-ordering func #:dependent? [dependent? #f])
  ((if dependent? dependent-ordering independent-ordering) func))

(define raw-variable? string?)
(struct variable+ordering (name order) #:extra-constructor-name with-ordering)
(define variable? (or/c raw-variable? variable+ordering?))

(define (variable-name v)
  (cond [(raw-variable? v) v]
        [(variable+ordering? v) (variable+ordering-name v)]))

(define lexicographic (make-ordering string<?))
(define lexicographic-ci (make-ordering string-ci<?))
(define (mean-of to-mean)
  (define ((mean-of/internal var) a b)
    (for/fold ([as '()] [bs '()] #:result (< (mean as) (mean bs)))
              ([(val to-mean) (in-data-frame (gr-data) var to-mean)]
               #:when to-mean)
      (cond [(equal? val a) (values (cons to-mean as) bs)]
            [(equal? val b) (values as (cons to-mean bs))]
            [else (values as bs)])))
  (make-ordering mean-of/internal #:dependent? #t))

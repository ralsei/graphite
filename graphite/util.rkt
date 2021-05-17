#lang racket
(require data-frame threading racket/dict racket/hash
         kw-utils/kw-hash-lambda
         (for-syntax syntax/parse))
(provide (all-defined-out))

(define (keyword->symbol s)
  (string->symbol (keyword->string s)))
(define (symbol->keyword s)
  (string->keyword (symbol->string s)))

(define-syntax (define-renderer stx)
  (syntax-parse stx
    [(_ (FN-NAME:id . ARGS:expr)
        ({~seq KEY:keyword VALUE:expr} ...)
        FN-BODY:expr ...)
     #'(define (FN-NAME . ARGS)
         (hash 'function (λ ()
                           FN-BODY ...)
               {~@ (keyword->symbol 'KEY) VALUE} ...))]))

(define (renderer-function r)
  (hash-ref r 'function))
(define (renderer-metadata r)
  (hash-remove r 'function))

(define-syntax (define-parameter stx)
  (syntax-parse stx
    [(_ NAME VALUE) #'(define NAME (make-parameter VALUE))]
    [(_ NAME) #'(define NAME (make-parameter #f))]
    [_ (raise-syntax-error 'define-parameter
                           (format "expected a name and a value, or just a name"))]))

(define-parameter gr-data)
(define-parameter gr-global-mapping)
(define-parameter gr-x-conv)
(define-parameter gr-y-conv)
(define-parameter gr-group)

(define-parameter gr-x-min)
(define-parameter gr-x-max)
(define-parameter gr-y-min)
(define-parameter gr-y-max)

(define (vector-remove-duplicates vec)
  (define seen (mutable-set))
  (for/vector ([v (in-vector vec)]
               #:unless (set-member? seen v))
    (set-add! seen v)
    v))

(define (possibilities data group)
  (~> (df-select data group)
      vector-remove-duplicates
      (vector-filter (λ (x) (and x #t)) _)))

(define (vector-reshape vec cols)
  (define len (vector-length vec))
  (define rows (exact-ceiling (/ len cols)))
  (for/vector ([y (in-range rows)])
    (for/vector ([x (in-range cols)])
      (define idx (+ x (* cols y)))
      (and (< idx len) (vector-ref vec idx)))))

(define (mapping-override mapping local-mapping)
  (hash-union mapping local-mapping #:combine (λ (x y) y)))

(define (in-infinite val)
  (in-cycle (in-value val)))

(define (in-data-frame* data . series)
  (define generators
    (for/list ([s (in-list series)])
      (cond [s (in-data-frame data s)]
            [else (in-infinite s)])))

  (cond [(empty? generators) (in-parallel '())]
        [else (apply in-parallel generators)]))

(define (hash-remove* hsh keys)
  (for/fold ([ret hsh])
            ([k (in-list keys)])
    (hash-remove ret k)))

; if a1 becomes b1 and a2 becomes b2, then what should a become?
(define (convert a1 b1 a2 b2 a)
  (/ (+ (* b1 (- a2 a)) (* b2 (- a a1))) (- a2 a1)))

(define run-renderer
  (kw-hash-lambda args #:kws kw-hash
    (define renderer (hash-ref kw-hash '#:renderer))
    (define mapping (hash-ref kw-hash '#:mapping))
    (define-values (_ kws) (procedure-keywords renderer))
    (keyword-apply/dict
     renderer
     (mapping-override (hash-remove* kw-hash '(#:renderer #:mapping))
                       (for/hash ([(k v) (in-hash mapping)]
                                  #:when (and (member (symbol->keyword k) kws)
                                              (not (and (member k '(x y)) #t))))
                         (values (symbol->keyword k) v)))
     args)))

#lang racket
(require data-frame threading racket/dict racket/hash
         kw-utils/kw-hash-lambda
         (for-syntax syntax/parse))
(provide (all-defined-out))

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

(define (symbol->keyword s)
  (string->keyword (symbol->string s)))

(define used-names
  (set 'x 'y 'facet 'discrete-color))

(define run-renderer
  (kw-hash-lambda args #:kws kw-hash
    (match-define `(,renderer ,mapping . ,rst) args)
    (keyword-apply/dict
     renderer
     (mapping-override (for/hash ([(k v) (in-hash mapping)]
                                  #:when (not (set-member? used-names k)))
                         (values (symbol->keyword k) v))
                       kw-hash)
     rst)))

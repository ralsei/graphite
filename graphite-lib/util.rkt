#lang racket
(require data-frame threading racket/hash plot/utils pict
         (for-syntax syntax/parse))
(provide (all-defined-out))

(define (keyword->symbol s)
  (string->symbol (keyword->string s)))
(define (symbol->keyword s)
  (string->keyword (symbol->string s)))

; thanks sorawee: https://groups.google.com/g/racket-users/c/3_Vc3t0fTGs/m/HpLZQCwADQAJ
(define-syntax-rule (lambda/kw (kws kw-args . rst) body ...)
  (let ([f (λ (kws kw-args . rst) body ...)])
    (define-values (ignored used-kws) (procedure-keywords f))
    (make-keyword-procedure
     (λ (actual/kws actual/kw-args . actual/rest)
       (define-values (used-kws+kw-args left-kws+kw-args)
         (partition (λ (e) (member (car e) used-kws))
                    (map cons actual/kws actual/kw-args)))
       (keyword-apply f
                      (map car used-kws+kw-args)
                      (map cdr used-kws+kw-args)
                      (list* (map car left-kws+kw-args)
                             (map cdr left-kws+kw-args)
                             actual/rest))))))

(define-syntax-rule (define/kw (f . rst) body ...)
  (define f (lambda/kw rst body ...)))

(define-syntax (alist stx)
  (syntax-parse stx
    [(_ {~seq KEY:expr VALUE:expr} ...)
     #'(list {~@ (cons KEY VALUE)} ...)]))

(define (alist-remove-false alist)
  (match alist
    ['() '()]
    [`((,k . #f) . ,rst) (alist-remove-false rst)]
    [`(,p . ,rst) (cons p (alist-remove-false rst))]))

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

(define (in-infinite val)
  (in-cycle (in-value val)))

(define (in-data-frame* data . series)
  (define generators
    (for/list ([s (in-list series)])
      (cond [s (in-data-frame data s)]
            [else (in-infinite s)])))

  (cond [(empty? generators) (in-parallel '())]
        [else (apply in-parallel generators)]))

(define (hash-remove* hsh . keys)
  (for/fold ([ret hsh])
            ([k (in-list keys)])
    (hash-remove ret k)))

; if a1 becomes b1 and a2 becomes b2, then what should a become?
(define (convert a1 b1 a2 b2 a)
  (/ (+ (* b1 (- a2 a)) (* b2 (- a a1))) (- a2 a1)))

(define/kw (run-renderer kws kw-args #:renderer renderer
                         #:kws given-kws #:kw-args given-kwargs . rst)
  (keyword-apply/dict renderer
                      (hash-union (make-immutable-hash (map cons kws kw-args))
                                  (make-immutable-hash (map cons given-kws given-kwargs))
                                  #:combine (λ (_ y) y))
                      rst))

(define (background-rectangle width height)
  (colorize (filled-rectangle width height)
            (->color (plot-background))))

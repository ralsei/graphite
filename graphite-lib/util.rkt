#lang racket/base
(require (for-syntax racket/base syntax/parse)
         data-frame
         pict
         plot/no-gui
         plot/utils
         racket/dict
         racket/hash
         racket/list
         racket/match
         racket/math
         racket/set
         racket/vector
         threading
         "parameters.rkt")

(provide (all-defined-out)
         (all-from-out "parameters.rkt"))

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

(define (in-hash/sort hsh [cmpfn #f])
  (define sorted (try-sort-keys (hash->list hsh) cmpfn))
  (in-parallel (map car sorted) (map cdr sorted)))

; straight out of hash.ss
; https://github.com/racket/racket/blob/master/racket/src/cs/rumble/hash.ss#L500
(define (try-sort-keys keys cmpfn)
  (cond [cmpfn (sort keys cmpfn #:key car)]
        [(andmap (λ (p) (orderable? (car p))) keys)
         (sort keys orderable<? #:key car)]
        [else keys]))

(define (orderable-major v)
  (cond [(boolean? v)    0]
        [(char? v)       1]
        [(real? v)       2]
        [(symbol? v)     3]
        [(keyword? v)    4]
        [(string? v)     5]
        [(null? v)       6]
        [(void? v)       7]
        [(eof-object? v) 8]
        [else #f]))

(define (orderable? v) (and (orderable-major v) #t))

(define (orderable<? a b)
  (let ([am (orderable-major a)]
        [bm (orderable-major b)])
    (cond [(or (not am) (not bm)) #f]
          [(= am bm)
           (cond [(boolean? a) (not a)]
                 [(char? a) (char<? a b)]
                 [(real? a) (< a b)]
                 [(symbol? a)
                  (cond [(symbol-interned? a)
                         (and (symbol-interned? b)
                              (symbol<? a b))]
                        [(symbol-interned? b) #t]
                        [(symbol-unreadable? a)
                         (and (symbol-unreadable? b)
                              (symbol<? a b))]
                        [(symbol-unreadable? b) #t]
                        [else (symbol<? a b)])]
                 [(keyword? a) (keyword<? a b)]
                 [(string? a) (string<? a b)]
                 [else #f])]
          [else (< am bm)])))

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

(define (vl-append-backwards offset pict-a pict-b)
  (lb-superimpose pict-b (inset pict-a 0 0 0 (- (pict-height pict-b) (- offset)))))

(define no-renderer (invisible-rect #f #f #f #f))

(define (build-list* n proc rst)
  (apply list* (append (build-list n proc) (list rst))))

#lang racket
(require kw-utils/kw-hash-lambda plot/utils
         "util.rkt")
(provide graphite-renderer/c
         aes? aes-with/c aes-containing/c)

(define graphite-renderer/c
  (and/c hash?
         (λ (v) (hash-has-key? v 'function))
         (hash/dc [k symbol?]
                  [v (k) (if (eq? k 'function)
                             (-> (treeof (or/c renderer2d? nonrenderer?)))
                             any/c)])))

(define aes? (and/c hash? hash-equal? immutable?))

; aes must have these values, with these contracts
(define aes-with/c
  (kw-hash-lambda args #:kws kw-hash
    (when (not (empty? args))
      (error 'aes-with/c "called with non-keyword argument"))
    (λ (aes)
      (and (aes? aes)
           (for/and ([(k v) (in-hash kw-hash)])
             (define sym (keyword->symbol k))
             (and (hash-has-key? aes sym)
                  ((hash-ref kw-hash sym) v)))))))

; aes optionally has these values, with these contracts
(define aes-containing/c
  (kw-hash-lambda args #:kws kw-hash
    (when (not (empty? args))
      (error 'aes-containing/c "called with non-keyword argument"))
    (λ (aes)
      (and (aes? aes)
           (for/and ([(k v) (in-hash aes)])
             (define sym (symbol->keyword k))
             (if (hash-has-key? kw-hash sym)
                 ((hash-ref kw-hash sym) v)
                 #t))))))

#lang racket
(require plot/utils "util.rkt")
(provide graphite-renderer/c
         aes? aes-with/c aes-containing/c)

(define graphite-renderer/c
  (and/c hash?
         (λ (v) (hash-has-key? v 'function))
         (hash/dc [k (or/c symbol? parameter?)]
                  [v (k) (if (eq? k 'function)
                             (-> (treeof (or/c renderer2d? nonrenderer?)))
                             any/c)])))

(define aes? (and/c hash? hash-equal? immutable?))

; aes must have these values, with these contracts
(define/kw (aes-with/c kws kw-args . rst)
  (when (not (empty? rst))
    (error 'aes-with/c "called with non-keyword argument"))
  (define kw-hash (make-immutable-hash (map cons kws kw-args)))
  (λ (aes)
    (and (aes? aes)
         (for/and ([(k v) (in-hash kw-hash)])
           (define sym (keyword->symbol k))
           (and (hash-has-key? aes sym)
                ((hash-ref kw-hash sym) v))))))

; aes optionally has these values, with these contracts
(define/kw (aes-containing/c kws kw-args . rst)
  (when (not (empty? rst))
    (error 'aes-containing/c "called with non-keyword argument"))
  (define kw-hash (make-immutable-hash (map cons kws kw-args)))
  (λ (aes)
    (and (aes? aes)
         (for/and ([(k v) (in-hash aes)])
           (define sym (symbol->keyword k))
           (if (hash-has-key? kw-hash sym)
               ((hash-ref kw-hash sym) v)
               #t)))))

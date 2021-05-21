#lang racket
(require plot/utils
         "util.rkt"
         (for-syntax syntax/parse racket/syntax))
(provide
 (contract-out
  [struct graphite-renderer ((function (-> (treeof (or/c renderer2d? nonrenderer?))))
                             (metadata (listof (cons/c parameter? any/c))))])
 define-renderer with-metadata)

(struct graphite-renderer (function metadata))

(define-syntax (define-renderer stx)
  (syntax-parse stx
    [(_ (FN-NAME:id #:kws KWS:id #:kw-args KW-ARGS:id . ARGS:expr)
        ({~seq KEY:keyword VALUE:expr} ...)
        FN-BODY:expr ...)
     #:with (KEY-PARAM ...) (map (λ (x)
                                   (format-id x "plot-~a" (keyword->string (syntax->datum x))))
                                 (attribute KEY))
     #'(define/kw (FN-NAME KWS KW-ARGS . ARGS)
         (graphite-renderer (λ () FN-BODY ...)
                            (alist {~@ KEY-PARAM VALUE} ...)))]))

(define (call-while-parameterizing-with metadata thnk)
  (match metadata
    [`((,x . ,y) . ,rst)
     (parameterize ([x y])
       (call-while-parameterizing-with rst thnk))]
    ['() (thnk)]))

(define-syntax-rule (with-metadata metadata body ...)
  (call-while-parameterizing-with metadata (λ () body ...)))

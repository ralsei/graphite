#lang racket
(require racket/hash bestfit data-frame plot/pict)
(provide fit)

(define ((fit #:method [method 'linear] #:mapping [local-mapping (make-hash)])
         #:data data #:gmapping mapping #:x-conv x-conv #:y-conv y-conv)
 (define aes (hash-union mapping local-mapping #:combine (λ (x y) x)))
 (define fit-function
   (match method
     ['linear linear-fit]
     ['exp exp-fit]
     ['power power-fit]
     ['log log-fit]))
 (define fit-line
   (for/fold ([xs '()] [ys '()]
              #:result (fit-function xs ys))
             ([(x y) (in-data-frame data (hash-ref aes 'x) (hash-ref aes 'y))]
              #:when (and x y))
     (values (cons (exact->inexact (x-conv x)) xs) (cons (exact->inexact (y-conv y)) ys))))
 (function fit-line #:width (hash-ref aes 'width 1)))

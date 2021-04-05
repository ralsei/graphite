#lang racket
(require bestfit plot/pict
         "util.rkt")
(provide fit)

(define ((fit #:method [method 'linear] #:mapping [local-mapping (make-hash)]))
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define facet-on (hash-ref aes 'facet #f))
  (define fit-function
    (match method
       ['linear linear-fit]
       ['exp exp-fit]
       ['power power-fit]
       ['log log-fit]))
  (define fit-line
    (for/lists (xs ys #:result (fit-function xs ys))
               ([(x y facet) (in-data-frame* (gr-data) (hash-ref aes 'x) (hash-ref aes 'y) facet-on)]
                #:when (and x y)
                #:when (equal? facet (gr-group)))
      (values (exact->inexact ((gr-x-conv) x)) (exact->inexact ((gr-y-conv) y)))))
  (run-renderer #:renderer function
                #:mapping aes
                fit-line))

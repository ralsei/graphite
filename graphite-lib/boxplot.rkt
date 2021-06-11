#lang racket/base
(require fancy-app
         plot/no-gui
         plot/utils
         racket/contract/base
         "aes.rkt"
         "extern/box-and-whiskers.rkt"
         "renderer.rkt"
         "qualitative.rkt"
         "util.rkt")
(provide
 (contract-out [boxplot (->* ()
                             (#:invert? boolean?
                              #:gap (real-in 0 1)
                              #:box-color plot-color/c
                              #:box-style plot-brush-style/c
                              #:box-line-color plot-color/c
                              #:box-line-width (>=/c 0)
                              #:box-line-style plot-pen-style/c
                              #:box-alpha (real-in 0 1)
                              #:show-outliers? boolean?
                              #:outlier-color plot-color/c
                              #:outlier-sym point-sym/c
                              #:outlier-fill-color (or/c plot-color/c 'auto)
                              #:outlier-size (>=/c 0)
                              #:outlier-line-width (>=/c 0)
                              #:outlier-alpha (real-in 0 1)
                              #:show-whiskers? boolean?
                              #:whiskers-color plot-color/c
                              #:whiskers-width (>=/c 0)
                              #:whiskers-style plot-pen-style/c
                              #:whiskers-alpha (real-in 0 1)
                              #:show-median? boolean?
                              #:median-color plot-color/c
                              #:median-width (>=/c 0)
                              #:median-style plot-pen-style/c
                              #:median-alpha (real-in 0 1)
                              #:iqr-scale real?
                              #:mapping (aes-containing/c #:x string?
                                                          #:y string?
                                                          #:facet (or/c string? #f)))
                             graphite-renderer?)]))


(define (make-stats mapping iqr-scale invert?)
  (define-values (vs var->real real->var)
    (qualitative-iso (hash-ref mapping (if invert? 'y 'x))))

  (define list-tbl (make-hash))
  (for ([(x y facet) (in-data-frame* (gr-data) (hash-ref mapping 'x)
                                     (hash-ref mapping 'y)
                                     (hash-ref mapping 'facet #f))]
        #:when (and x y)
        #:when (equal? facet (gr-group)))
    (define conv-x ((gr-x-conv) x))
    (define conv-y ((gr-y-conv) y))
    (hash-update! list-tbl (if invert? conv-y conv-x)
                  (cons (if invert? conv-x conv-y) _) '()))

  (for/list ([s (in-vector vs)])
    (samples->bnw-data (hash-ref list-tbl s) #:iqr-scale iqr-scale)))

(define (do-invert? kws kw-args)
  (let ([v (assoc '#:invert? (map cons kws kw-args))])
    (and v (cdr v))))

(define-renderer (boxplot #:kws kws #:kw-args kw-args
                          #:iqr-scale [iqr-scale 1.5] #:mapping [local-mapping (aes)])
                 (#:x-ticks (and (not (do-invert? kws kw-args)) no-ticks)
                  #:y-ticks (and (do-invert? kws kw-args) no-ticks))
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define invert? (do-invert? kws kw-args))

  (cons
   (qualitative-ticks (hash-ref aes (if invert? 'y 'x))
                      (if invert? y-ticks x-ticks))
   (for/list ([v (in-list (make-stats aes iqr-scale invert?))]
              [c (in-naturals)])
     (list (run-renderer #:renderer box-and-whiskers
                         #:kws kws #:kw-args kw-args
                         #:x c
                         v)))))

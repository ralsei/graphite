#lang racket/base
(require fancy-app
         pict
         plot/no-gui
         plot/utils
         racket/contract/base
         racket/format
         racket/match
         "aes.rkt"
         "qualitative.rkt"
         "renderer.rkt"
         "util.rkt")
(provide
 (contract-out [col (->* ()
                         (#:x-min (or/c rational? #f)
                          #:x-max (or/c rational? #f)
                          #:y-min (or/c rational? #f)
                          #:y-max (or/c rational? #f)
                          #:color plot-color/c
                          #:style plot-brush-style/c
                          #:line-color plot-color/c
                          #:line-width (>=/c 0)
                          #:line-style plot-pen-style/c
                          #:alpha (real-in 0 1)
                          #:label (or/c string? pict? #f)
                          #:gap real?
                          #:baseline real?
                          #:mapping (aes-containing/c #:x string?
                                                      #:y string?
                                                      #:discrete-color (or/c string? #f)
                                                      #:facet (or/c string? #f)))
                         graphite-renderer?)]))

(define-renderer (col #:kws kws #:kw-args kw-args
                      #:gap [gap 0] #:baseline [baseline 0] #:mapping [local-mapping (aes)]) ()
  (define aes (mapping-override (gr-global-mapping) local-mapping))
  (define discrete-color (hash-ref aes 'discrete-color #f))
  (define x-qualitative? (qualitative? aes 'x))

  (define-values (x-vs x->real real->x) (variable-iso aes 'x))

  (define tbl (make-hash))
  (for ([(x y strat facet)
         (in-data-frame* (gr-data) (hash-ref aes 'x) (hash-ref aes 'y)
                         discrete-color (hash-ref aes 'facet #f))]
        #:when (and x y)
        #:when (equal? facet (gr-group)))
    (hash-update! tbl strat (cons (vector (x->real ((gr-x-conv) x)) ((gr-y-conv) y)) _) null))

  (define (build-ivls pts)
    (for/vector ([pt (in-list pts)])
      (match-define (vector x y) pt)
      (vector (ivl (+ x (* 1/2 gap)) (- (add1 x) (* 1/2 gap)))
              (ivl baseline y))))

  (list* (if x-qualitative? (qualitative-ticks aes 'x x-ticks #:start-at 1/2) no-renderer)
         (for/list ([(strat pts) (in-hash/sort tbl)]
                    [color-n (in-naturals)])
           (run-renderer #:renderer rectangles #:kws kws #:kw-args kw-args
                         #:color color-n
                         #:label (and discrete-color (~a strat))
                         (build-ivls pts)))))

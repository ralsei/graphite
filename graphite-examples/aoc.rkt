#lang racket
(require data-frame fancy-app graphite
         (only-in plot date-ticks)
         "aoc-lib.rkt")
(provide (all-defined-out))

(define (hash-ref* hsh path [failure (thunk (error "no such key" path))])
  (define (->symbol el)
    (match el
      [(? string?) (string->symbol el)]
      [(? number?) (string->symbol (number->string el))]
      [(? symbol?) el]))
  (define (do-failure)
    (if (procedure? failure) (failure) failure))
  (match path
    ['() (do-failure)]
    [(cons (app ->symbol fst) '()) (hash-ref hsh fst failure)]
    [(cons (app ->symbol fst) rst)
     (if (hash-has-key? hsh fst)
         (hash-ref* (hash-ref hsh fst) rst failure)
         (do-failure))]))

(define (generate-leaderboard-df raw)
  (define members (hash-ref raw 'members))
  (define max-pts (hash-count members))
  (define member-names (make-hash))

  (struct tl-entry [mid day level] #:transparent)
  (define timeline (make-hash))

  (for ([(mid-in val) (in-hash members)])
    (define mid (string->number (symbol->string mid-in)))
    (define name (hash-ref val 'name))
    (hash-set! member-names mid name)
    (for* ([day (in-range 1 26)] [level (in-range 1 3)])
      (match (hash-ref* val (list 'completion_day_level day level 'get_star_ts) #f)
        [#f (void)]
        [(app string->number (? number? time))
         (hash-update! timeline time (lambda (v) (cons (tl-entry mid day level) v)) '())])))

  (define timestamps (sort (hash-keys timeline) <))

  (define point-values (make-hash))
  (for* ([day (in-range 1 26)] [level (in-range 1 3)])
    (hash-set! point-values (cons day level)
               (if (= day 1) 0 max-pts)))

  (define point-tls (make-hash))
  (for ([(mid _) (in-hash member-names)])
    (hash-set! point-tls mid (vector)))

  (define (get-last-pts data)
    (if (vector-empty? data)
      0
      (vector-ref (vector-ref data (sub1 (vector-length data))) 1)))

  (define (set-pts! mid time pts)
    (hash-update! point-tls mid
                  (lambda (m-tl)
                    (vector-append m-tl (vector (vector time (get-last-pts m-tl))
                                                (vector time pts))))))

  (for ([time (in-list timestamps)])
    (for ([solve (in-list (hash-ref timeline time))])
      (match-define (tl-entry mid day level) solve)
      (define key (cons day level))
      (define m-tl (hash-ref point-tls mid))
      (define old-pts (get-last-pts m-tl))
      (define pts (hash-ref point-values key))
      (hash-set! point-values key (max 0 (sub1 pts)))
      (set-pts! mid time (+ old-pts pts))))

  ; hack
  (define now 1609005817)
  (for ([(mid data) (in-hash point-tls)])
    (define pts (get-last-pts data))
    (set-pts! mid now pts))

  (define-values (names times points)
    (for/fold ([names (vector)] [times (vector)] [points (vector)])
              ([(id x-y) (in-hash point-tls)])
      (define this-times (vector-map (vector-ref _ 0) x-y))
      (define this-points (vector-map (vector-ref _ 1) x-y))
      (unless (equal? (vector-length this-times) (vector-length this-points))
        (error "oopsie woopsie"))

      (values (vector-append (make-vector (vector-length this-times)
                                          (hash-ref member-names id)) names)
              (vector-append (vector-map (vector-ref _ 0) x-y) times)
              (vector-append (vector-map (vector-ref _ 1) x-y) points))))

  (define result (make-data-frame))
  (df-add-series result (make-series "name"
                                     #:data names
                                     #:cmpfn #f
                                     #:na #f
                                     #:contract string?))
  (df-add-series result (make-series "time"
                                     #:data times
                                     #:cmpfn #f
                                     #:na #f
                                     #:contract number?))
  (df-add-series result (make-series "points"
                                     #:data points
                                     #:cmpfn #f
                                     #:na #f
                                     #:contract number?))

  result)

(module+ main
  ; set in .envrc
  (define raw-data (aoc-fetch-leaderboard (getenv "AOC_YEAR")
                                          (getenv "AOC_LEADERBOARD")
                                          (getenv "AOC_SESSION")))

  (define result (graph #:data (generate-leaderboard-df raw-data)
                        #:width 2400
                        #:height 1200
                        #:x-label "date/time"
                        #:y-label "points"
                        #:mapping (aes #:x "time" #:y "points" #:discrete-color "name")
                        #:x-max 1609005817
                        #:x-ticks (date-ticks)
                        (lines #:mapping (aes #:width 2))))
  (save-pict result "./aoc.png"))

#lang racket/base
(require pict
         plot/no-gui
         racket/match
         racket/math
         "util.rkt"
         "with-area.rkt")
(provide px->pt pt->px title add-title add-all-titles add-facet-label)

(define (px->pt px) (* px 3/4))
(define (pt->px pt) (* pt 4/3))

(define (->pict-font-style)
  (or (plot-font-face) (plot-font-family) null))

(define (title content
               #:style [style #f]
               #:size [size (round (pt->px (plot-font-size)))]
               #:angle [angle 0])
  (if content
      (text content (if style
                        (cons style (->pict-font-style))
                        (->pict-font-style))
            size angle)
      (blank)))

(define (add-title title-text side position pct #:offset [offset 0])
  (define angle
    (match side
      ['left (/ pi 2)]
      ['right (/ (* 3 pi) 2)]
      [_ 0]))

  (define ((flip fn) a b)
    (fn b a))
  (define combiner
    (match* (side position)
      [('top 'left) vl-append]
      [('top 'center) vc-append]
      [('top 'right) vr-append]
      [('left 'top) ht-append]
      [('left 'center) hc-append]
      [('left 'bottom) hb-append]
      [('right 'top) (flip ht-append)]
      [('right 'center) (flip hc-append)]
      [('right 'bottom) (flip hb-append)]
      [('bottom 'left) (flip vl-append)]
      [('bottom 'center) (flip vc-append)]
      [('bottom 'right) (flip vr-append)]))

  (define top-offset
    (if (and (not (negative? offset))
             (or (eq? side 'left) (eq? side 'right)))
        offset
        0))
  (define bot-offset
    (if (and (negative? offset)
             (or (eq? side 'left) (eq? side 'right)))
        (- offset)
        0))
  (define left-offset
    (if (and (not (negative? offset))
             (or (eq? side 'top) (eq? side 'bottom)))
        offset
        0))
  (define right-offset
    (if (and (negative? offset)
             (or (eq? side 'top) (eq? side 'bottom)))
        (- offset)
        0))

  (combiner (inset (title title-text #:angle angle)
                   left-offset top-offset right-offset bot-offset)
            pct))

(define (add-facet-label plot-pict)
  (match-define-values ((app inexact->exact left-extras)
                        (app inexact->exact right-extras) _ _)
    (plot-extras-size plot-pict))
  (define titled
    (add-title
     (gr-facet-label) 'top 'center #:offset (- (+ left-extras (pt->px (plot-font-size)))
                                               right-extras)
     plot-pict))
  (define bg (background-rectangle (pict-width titled) (pict-height titled)))
  (cc-superimpose bg titled))

(define (add-all-titles regular-pict)
  ; XXX: center x/y labels -- this requires metrics info which gets lost!!
  (define titled
    (add-title
     (gr-title) 'top 'center
     (add-title
      (gr-x-label) 'bottom 'right #:offset -30
      (add-title
       (gr-y-label) 'left 'top #:offset (+ 10 (pt->px (plot-font-size)))
       regular-pict))))
  (define bg (background-rectangle (pict-width titled) (pict-height titled)))
  (cc-superimpose bg titled))

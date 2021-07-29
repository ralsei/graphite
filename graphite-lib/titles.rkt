#lang racket/base
(require pict
         plot/no-gui
         racket/format
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
  (cond [content
         (define initial-title
           (text (~a content)
                 (if style
                     (cons style (->pict-font-style))
                     (->pict-font-style))
                 size))
         (define w (pict-height initial-title))
         (rotate (inset initial-title 0 (/ (- (ceiling w) w) 2)) angle)]
        [else (blank)]))

(define (add-title title-text side position pct #:v-offset [v-offset 0] #:h-offset [h-offset 0])
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

  (combiner (inset (title title-text #:angle angle)
                   (if (negative? h-offset) (- h-offset) 0)
                   (if (not (negative? v-offset)) v-offset 0)
                   (if (not (negative? h-offset)) h-offset 0)
                   (if (negative? v-offset) (- v-offset) 0))
            pct))

; NOTE on facet labels:
; when faceting, we want a common baseline to work on.
; the way facet labels are added, we account for the top-extras in order to _induce_ that baseline.
; this way we have something common to align on and can use lt-superimpose
;
; we also do not add a background because we want transparency for when bottom extras overlap with the
; facet title. the background gets added at the end of main/facet-plot
;
; we ALSO normalize the left and right extras, so we get a common baseline to work with for
; cc-superimpose.
(define (add-facet-label group plot-pict)
  (match-define-values ((app inexact->exact left-extras)
                        (app inexact->exact right-extras)
                        _
                        (app inexact->exact top-extras))
                       (plot-extras-size plot-pict))
  (define add-left-extras
    (if (< left-extras right-extras)
        (- right-extras left-extras)
        0))
  (define add-right-extras
    (if (< right-extras left-extras)
        (- left-extras right-extras)
        0))
  (define t (title group))
  (cb-superimpose (inset plot-pict add-left-extras 0 add-right-extras 0)
                  (inset t add-left-extras 0 add-right-extras
                         (- (pict-height plot-pict) top-extras))))

(define (add-all-titles regular-pict #:x-offset [x-offset 0] #:y-offset [y-offset 0])
  ; XXX: center x/y labels -- this requires metrics info which gets lost!!
  (define titled
    (add-title
     (gr-title) 'top 'center
     (add-title
      (gr-x-label) 'bottom 'center #:h-offset x-offset
      (add-title
       (gr-y-label) 'left 'center #:v-offset y-offset
       regular-pict))))
  (define bg (background-rectangle (pict-width titled) (pict-height titled)))
  (cc-superimpose bg titled))

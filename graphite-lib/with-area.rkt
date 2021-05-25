#lang racket
(require plot/no-gui pict)
(provide plot-extras-size plot-with-area)

(define (plot-extras-size plotpict)
  (match-define (vector (vector x-min x-max)
                        (vector y-min y-max))
    (plot-pict-bounds plotpict))
  (match-define (vector x-left y-bottom)
    ((plot-pict-plot->dc plotpict) (vector x-min y-min)))
  (match-define (vector x-right y-top)
    ((plot-pict-plot->dc plotpict) (vector x-max y-max)))

  (values x-left                              ; left
          (- (pict-width plotpict) x-right)   ; right
          (- (pict-height plotpict) y-bottom) ; bottom
          y-top))                             ; top

(define (plot-with-area plot-thunk area-width area-height)
  (match-define-values ((app inexact->exact left-extras)
                        (app inexact->exact right-extras)
                        (app inexact->exact bot-extras)
                        (app inexact->exact top-extras))
    (plot-extras-size (plot-thunk)))

  (parameterize ([plot-width (+ area-width left-extras right-extras)]
                 [plot-height (+ area-height bot-extras top-extras)])
    (plot-thunk)))

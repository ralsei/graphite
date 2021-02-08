#lang racket
(require data-frame plot)
(provide dataframe)

; dataframe : [A B C D] dataframe string string (A -> B) (C -> D) -> renderer2d
; given a dataframe, the data to use for the x-axis, the data to use
; for the y-axis, and conversion functions for both, plot it
(define (dataframe df x-axis y-axis
                   #:x-conv [x-conv values] #:y-conv [y-conv values]
                   #:color [color 'black] #:label [label #f])
  (points
   (for/vector ([(x y) (in-data-frame df x-axis y-axis)]
                #:when (and x y))
     (vector (x-conv x) (y-conv y)))
   #:color color #:label label))

#lang racket
(require data-frame plot)
(provide df-plot)

; df-plot : [A B C D] dataframe string string (A -> B) (C -> D) -> plot
; given a dataframe, the data to use for the x-axis, the data to use
; for the y-axis, and conversion functions for both, plot it
(define (df-plot df x-axis y-axis #:x-conv [x-conv values] #:y-conv [y-conv values])
  (plot (points
         (for/vector ([(x y) (in-data-frame df x-axis y-axis)])
           (vector (x-conv x) (y-conv y))))))

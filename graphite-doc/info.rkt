#lang info
(define collection "graphite-doc")
(define deps '("base"))
(define scribblings '(("graphite.scrbl" ())))
(define build-deps '("graphite-tutorial"
                     "loess"
                     "draw-doc"
                     "plot-gui-lib"
                     "simple-polynomial"
                     "data-frame"
                     "graphite-lib"
                     "pict-doc"
                     "pict-lib"
                     "plot-doc"
                     "plot-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "racket-doc"
                     "gregor"))

(define pkg-desc "Documentation for graphite")
(define version "1.0")

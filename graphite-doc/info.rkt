#lang info
(define collection "graphite-doc")
(define deps '("base"))
(define scribblings '(("graphite.scrbl" ())))
(define build-deps '("plot-gui-lib"
                     "simple-polynomial"
                     "data-frame"
                     "graphite-lib"
                     "graphite-tutorial"
                     "pict-doc"
                     "pict-lib"
                     "plot-doc"
                     "plot-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "racket-doc"))

(define pkg-desc "Documentation for graphite")
(define version "1.0")

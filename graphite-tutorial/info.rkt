#lang info
(define collection "graphite-tutorial")
(define deps '("base"))
(define scribblings '(("graphite-tutorial.scrbl" (multi-page))))
(define build-deps '("plot-doc"
                     "data-frame"
                     "graphite-doc"
                     "graphite-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "sawzall-doc"
                     "sawzall-lib"
                     "scribble-lib"
                     "threading-doc"
                     "threading-lib"))

(define pkg-desc "A guided tour for graphite")
(define version "1.0")

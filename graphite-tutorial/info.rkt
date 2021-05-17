#lang info
(define collection "graphite-tutorial")
(define deps '("base"))
(define scribblings '(("graphite-tutorial.scrbl" ())))
(define build-deps '("data-frame"
                     "graphite-doc"
                     "graphite-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))

(define pkg-desc "A guided tour for graphite")
(define version "1.0")

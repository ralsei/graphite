#lang info
(define collection "graphite-test")
(define deps '("draw-lib"
               "plot-gui-lib"
               "base"
               "data-frame"
               "graphite-lib"
               "plot-lib"
               "pict-lib"
               "rackunit-lib"))

(define build-deps '())

(define test-omit-paths '("./info.rkt" "./util.rkt"))
(define test-responsibles '((all hazel@knightsofthelambdacalcul.us)))

(define pkg-desc "Tests for graphite")
(define version "1.0")

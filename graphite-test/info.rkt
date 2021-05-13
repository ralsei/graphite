#lang info
(define collection "graphite-test")
(define deps '("base"
               "data-frame"
               "graphite"
               "plot-lib"
               "pict-lib"
               "rackunit-lib"))

(define build-deps '())

(define test-omit-paths '("./info.rkt"
			  "./util.rkt"))
(define test-responsibles '((all hazel@knightsofthelambdacalcul.us)))

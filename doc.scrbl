#lang scribble/manual

@(require scribble/examples)

@(define ev (make-evaluator))
@(ev '(begin (require plot data-frame)))

@title{Fun with plots}

@section{Chicago}

@examples[#:eval ev
(plot (function (lambda (x) x)))
]
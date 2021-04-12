#lang scribble/manual

@(require scribble/example racket/sandbox)

@(define ev (make-evaluator 'racket/base))
@(ev '(begin (require plot/pict data-frame)))

@title{Fun with plots}

@section{Chicago}

@examples[#:eval ev
          (plot (function (lambda (x) x) -2 2))]

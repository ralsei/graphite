#lang scribble/manual
@(require scribble/example (for-label racket data-frame graphite))
@declare-exporting[graphite]

@title{Graphite: A data visualization library}

@table-of-contents[]

@section[#:tag "graphing-procedures"]{Graphing Procedures}

@defmodule*/no-declare[(graphite) #:link-target? #f]

@defproc[(graph [#:data data data-frame?]
                [#:mapping mapping aes?]
                [#:width width (or/c rational? #f) (plot-width)]
                [#:height height (or/c rational? #f) (plot-height)]
                [#:title title (or/c string? pict? #f) (plot-title)]
                [#:x-label x-label (or/c string? pict? #f) (plot-x-label)]
                [#:x-transform x-transform (or/c invertible-function? #f) #f]
                [#:x-ticks x-ticks ticks? (plot-x-ticks)]
                [#:x-conv x-conv (or/c (-> any/c real?) #f) #f]
                [#:x-min x-min (or/c rational? #f) #f]
                [#:x-max x-max (or/c rational? #f) #f]
                [#:y-label y-label (or/c string? pict? #f) (plot-y-label)]
                [#:y-transform y-transform (or/c invertible-function? #f) #f]
                [#:y-ticks y-ticks ticks? (plot-y-ticks)]
                [#:y-conv y-conv (or/c (-> any/c real?) #f) #f]
                [#:y-min y-min (or/c rational? #f) #f]
                [#:y-max y-max (or/c rational? #f) #f]
                [#:legend-anchor legend-anchor legend-anchor/c (plot-legend-anchor)]
                [renderer graphite-renderer?] ...)
         pict?]{
  Does the thing.
}

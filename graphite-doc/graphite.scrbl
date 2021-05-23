#lang scribble/manual
@(require scribble/example (for-label racket plot/utils
                                      pict data-frame graphite simple-polynomial
                                      (except-in plot points lines density
                                                      renderer2d? nonrenderer?)))

@(define ev
   (let ([eval (make-base-eval)])
     (eval '(begin
              (require data-frame
                       graphite
                       plot/utils
                       threading
                       racket/list)))
     (eval '(random-seed 1337))
     eval))

@title{Graphite: A data visualization library}
@author{@(author+email "Hazel Levine" "hazel@knightsofthelambdacalcul.us")}

@defmodule[graphite]

Graphite is a library designed for producing specific kinds of common graphs/plots, while
making decisions about the data being plotted. Graphite is designed to switch between different
types of plots relatively seamlessly, without changing anything about the underlying data's
structure.

Graphite is built on top of, and does not replace, @racketmodname[plot]. For many applications (e.g.
3D plotting, continuous data, interactive plots, etc), @racketmodname[plot] will be a far better fit.

A tutorial on @racketmodname[graphite] is also available;
@other-doc['(lib "graphite-tutorial/graphite-tutorial.scrbl")].

@table-of-contents[]

@section[#:tag "graphing-procedures"]{Graphing Procedures}

@defproc[(graph [#:data data data-frame?]
                [#:mapping mapping aes?]
                [#:width width (or/c rational? #f) (plot-width)]
                [#:height height (or/c rational? #f) (plot-height)]
                [#:title title (or/c string? pict? #f) (plot-title)]
                [#:x-label x-label (or/c string? pict? #f) #f]
                [#:x-transform x-transform (or/c transform? #f) #f]
                [#:x-conv x-conv (or/c (-> any/c real?) #f) #f]
                [#:x-min x-min (or/c rational? #f) #f]
                [#:x-max x-max (or/c rational? #f) #f]
                [#:y-label y-label (or/c string? pict? #f) #f]
                [#:y-transform y-transform (or/c transform? #f) #f]
                [#:y-conv y-conv (or/c (-> any/c real?) #f) #f]
                [#:y-min y-min (or/c rational? #f) #f]
                [#:y-max y-max (or/c rational? #f) #f]
                [#:facet-wrap facet-wrap (or/c positive-integer? #f) #f]
                [#:legend-anchor legend-anchor legend-anchor/c (plot-legend-anchor)]
                [renderer graphite-renderer?] ...)
         pict?]{
  The primary graphing procedure, producing a @racket[pict?]. All positional arguments are
  @racket[graphite-renderer?]s to be plotted, as returned by @racket[points], @racket[histogram], et cetera.

  The required argument @tt{#:data} takes a @racket[data-frame?], as provided by the @racketmodname[data-frame]
  library. Note that the data being fed in must be @italic{tidy}, meaning that:
  @itemlist[
    @item{Every column is a variable.}
    @item{Every row is an observation.}
    @item{Every cell is a single value.}
  ]

  The required argument @tt{#:mapping} takes a @racket[aes?] that dictates aesthetics to be applied to
  every renderer in the specified tree. Generally, you will want at least an x-axis (@tt{#:x}).

  The @tt{x-conv} and @tt{y-conv} arguments, if given, perform pre-processing of the x-axis and y-axis variable
  (when said variables are not automatically determined). For example, if you wanted to place dates on the
  x-axis, this could be a function converting your preferred date format to seconds since the UNIX epoch.

  The @tt{x-transform} and @tt{y-transform} arguments, if given, take a @racket[transform?] to adjust the x and
  y axes, as well as the ticks. For example, if you wanted to place a logarithmic transform on the x-axis, you
  could specify @racket[logarithmic-transform]. Transforms are applied @italic{after} the respective @tt{x-conv}
  or @racket{y-conv} function, if present.

  When given, the @tt{x-min} (etc.) arguments determine the bounds of the plot, but not the bounds of the
  individual renderers. For this, the data should be trimmed before being passed in.
}

@defproc[(save-pict [pict pict?]
                    [path path-string?])
         exact-nonnegative-integer?]{
  Saves a @racket[pict?] to disk, at the given path. Supports saving as PNG, PDF, or SVG, depending on the file
  extension.
}

@section[#:tag "aesthetics"]{Aesthetic Mappings}

@defproc[(aes [#:<key> value any/c] ...) aes?]{
  Creates an aesthetic mapping.

  These objects are generally passed with the @tt{#:mapping} keyword to either the @racket[graph] procedure or
  to each individual @racket[graphite-renderer?] in the render tree. They dictate various aesthetics, dictating
  how to display the data (such as colors, variables, et cetera), with behavior being dictated by each renderer.
}

@defproc[(aes? [v any/c]) boolean?]{
  Determines if the input is an aesthetic mapping.
}

@defproc[(aes-with/c [#:<key> contract contract?] ...) contract?]{
  Determines if the aesthetic mapping has each @tt{key}, with each value satisfying
  the given contract.
}

@defproc[(aes-containing/c [#:<key> contract contract?] ...) contract?]{
  Determines if the aesthetic mapping optionally contains each @tt{key}, and if it does,
  that each value satisfies the given contract.
}

@section[#:tag "renderers"]{Renderers}

@defstruct*[graphite-renderer ([function (-> (treeof (or/c renderer2d? nonrenderer?)))]
                               [metadata (listof (cons/c parameter? any/c))])]{
  The result of each renderer.

  Contains both a thunk returning a @racket[plot] render tree, and an association list of parameters
  (generally @racket[plot] parameters) to values, used when a renderer requires certain parameters
  to be set.
}

@defproc[(points [#:x-min x-min (or/c rational? #f) #f]
                 [#:x-max x-max (or/c rational? #f) #f]
                 [#:y-min y-min (or/c rational? #f) #f]
                 [#:y-max y-max (or/c rational? #f) #f]
                 [#:sym sym point-sym/c (point-sym)]
                 [#:color color plot-color/c (point-color)]
                 [#:fill-color fill-color (or/c plot-color/c 'auto) 'auto]
                 [#:x-jitter x-jitter (>=/c 0) (point-x-jitter)]
                 [#:y-jitter y-jitter (>=/c 0) (point-y-jitter)]
                 [#:size size (>=/c 0) (point-size)]
                 [#:line-width line-width (>=/c 0) (point-line-width)]
                 [#:alpha alpha (real-in 0 1) (point-alpha)]
                 [#:label label (or/c string? pict? #f) #f]
                 [#:mapping local-mapping
                            (aes-containing/c #:x string?
                                              #:y string?
                                              #:facet (or/c string? #f)
                                              #:discrete-color (or/c string? #f)
                                              #:continuous-color (or/c string? #f))
                            (aes)])
         graphite-renderer?]{
  Returns a renderer that draws a set of points, for example, to draw a (randomized) scatter plot:
  @examples[#:eval ev #:label #f
    (define (random-data)
      (build-vector 50 (λ (_) (random -50 50))))

    (define df (make-data-frame))
    (df-add-series! df (make-series "x-var" #:data (random-data)))
    (df-add-series! df (make-series "y-var" #:data (random-data)))

    (graph #:data df
           #:mapping (aes #:x "x-var" #:y "y-var")
           (points))
  ]

  The optional @tt{#:discrete-color} aesthetic dictates a variable to split on by color, in discrete groups.

  Similarly, the @tt{#:continuous-color} aesthetic dictates a continuous (numeric) variable to split on by
  color. You likely want to use a continuous colormap for this.
}

@defproc[(fit [#:x-min x-min (or/c rational? #f) #f]
              [#:x-max x-max (or/c rational? #f) #f]
              [#:y-min y-min (or/c rational? #f) #f]
              [#:y-max y-max (or/c rational? #f) #f]
              [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
              [#:color color plot-color/c (line-color)]
              [#:width width (>=/c 0) (line-width)]
              [#:style style plot-pen-style/c (line-style)]
              [#:alpha alpha (real-in 0 1) (line-alpha)]
              [#:label label (or/c string? pict? #f) #f]
              [#:degree degree positive-integer? 1]
              [#:show-equation? show-equation? boolean? #f]
              [#:mapping local-mapping
                         (aes-containing/c #:x string?
                                           #:y string?
                                           #:facet (or/c string? #f))
                         (aes)])
         graphite-renderer?]{
  Makes a line of best fit. Internally, this uses the @racket[simple-polynomial] library's best
  fit method. For example:
  @examples[#:eval ev #:label #f
    (define noise '(1/9 -1/7 0 1/3 -1 1/9))
    (define df (make-data-frame))
    (df-add-series! df (make-series "x-var" #:data (build-vector 6 add1)))
    (df-add-series! df
      (make-series "y-var"
                   #:data (build-vector 6 (λ (x) (+ x (list-ref noise x))))))

    (graph #:data df
           #:mapping (aes #:x "x-var" #:y "y-var")
           (points)
           (fit #:show-equation? #t))
  ]

  The optional @tt{#:degree} argument specifies the degree of the fit line (2 for a second-degree
  polynomial, et cetera).

  The optional @tt{#:show-equation?} argument specifies whether to show the full fit equation in the
  legend.
}

@defproc[(lines [#:x-min x-min (or/c rational? #f) #f]
                [#:x-max x-max (or/c rational? #f) #f]
                [#:y-min y-min (or/c rational? #f) #f]
                [#:y-max y-max (or/c rational? #f) #f]
                [#:color color plot-color/c (line-color)]
                [#:width width (>=/c 0) (line-width)]
                [#:style style plot-pen-style/c (line-style)]
                [#:alpha alpha (real-in 0 1) (line-alpha)]
                [#:label label (or/c string? pict? #f) #f]
                [#:mapping local-mapping
                           (aes-containing/c #:x string?
                                             #:y string?
                                             #:facet (or/c string? #f)
                                             #:discrete-color (or/c string? #f))
                           (aes)])
         graphite-renderer?]{
  Renders some lines connecting the points of the input sequence. This is useful for plotting a time
  series.

  As an example, consider a random walk, adapted from the @racketmodname[plot] documentation:
  @examples[#:eval ev #:label #f
    (define df (make-data-frame))
    (define-values (xs ys)
      (for/fold ([xs (list 0)] [ys (list 0)])
                ([i (in-range 1 200)])
        (values (cons i xs) (cons (+ (first ys) (* 1/100 (- (random) 1/2))) ys))))
    (df-add-series! df (make-series "x-var" #:data (list->vector xs)))
    (df-add-series! df (make-series "y-var" #:data (list->vector ys)))

    (graph #:data df
           #:mapping (aes #:x "x-var" #:y "y-var")
           (lines #:label "Random walk"))
  ]
}

@defproc[(bar [#:x-min x-min (or/c rational? #f) 0]
              [#:x-max x-max (or/c rational? #f) #f]
              [#:y-min y-min (or/c rational? #f) 0]
              [#:y-max y-max (or/c rational? #f) #f]
              [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
              [#:skip skip (>=/c 0) (discrete-histogram-skip)]
              [#:invert? invert? boolean? (discrete-histogram-invert?)]
              [#:color color plot-color/c (rectangle-color)]
              [#:style style plot-brush-style/c (rectangle-style)]
              [#:line-color line-color plot-color/c (rectangle-line-color)]
              [#:line-width line-width (>=/c 0) (rectangle-line-width)]
              [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
              [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
              [#:label label (or/c string? pict? #f) #f]
              [#:add-ticks? add-ticks? boolean? #t]
              [#:far-ticks? far-ticks? boolean? #f]
              [#:mode mode (or/c 'count 'prop) 'count]
              [#:group-gap group-gap (>=/c 0) 1]
              [#:mapping local-mapping
                         (aes-containing/c #:x string?
                                           #:facet (or/c string? #f)
                                           #:group (or/c string? #f))
                         (aes)])
         graphite-renderer?]{
  Renders a bar chart.

  The @tt{#:mode} argument dictates whether the y-axis should be the count of observations by the x-axis
  (@tt{'count}), or the relative frequency of those observations (@tt{'prop}).

  The optional @tt{#:group} aesthetic dictates whether the bar should be "dodged", with each bar
  being broken up into bars based on the group. If this is enabled, the @tt{#:group-gap} argument dictates
  the space between each sub-chart.
}

@defproc[(stacked-bar [#:x-min x-min (or/c rational? #f) #f]
                      [#:x-max x-max (or/c rational? #f) #f]
                      [#:y-min y-min (or/c rational? #f) 0]
                      [#:y-max y-max (or/c rational? #f) #f]
                      [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
                      [#:skip skip (>=/c 0) (discrete-histogram-skip)]
                      [#:invert? invert? boolean? (discrete-histogram-invert?)]
                      [#:colors colors (plot-colors/c nat/c) (stacked-histogram-colors)]
                      [#:styles styles (plot-brush-styles/c nat/c) (stacked-histogram-styles)]
                      [#:line-colors line-colors (plot-colors/c nat/c) (stacked-histogram-line-colors)]
                      [#:line-widths line-widths (pen-widths/c nat/c) (stacked-histogram-line-widths)]
                      [#:line-styles line-styles (plot-pen-styles/c nat/c) (stacked-histogram-line-styles)]
                      [#:alphas alphas (alphas/c nat/c) (stacked-histogram-alphas)]
                      [#:labels labels (labels/c nat/c) '(#f)]
                      [#:add-ticks? add-ticks? boolean? #t]
                      [#:far-ticks? far-ticks? boolean? #f]
                      [#:mode mode (or/c 'count 'prop) 'count]
                      [#:mapping local-mapping
                                 (aes-containing/c #:x string?
                                                   #:facet (or/c string? #f)
                                                   #:group string?)
                                 (aes)])
         graphite-renderer?]{
  Renders a stacked bar chart, stratified by group.

  The @italic{mandatory} @tt{#:group} aesthetic dictates what variable each bar should be broken up by.

  The @tt{#:mode} argument dictates whether the y-axis should be the count of observations by the x-axis
  (@tt{'count}), or the relative frequency of those observations (@tt{'prop}). @tt{'prop} does not make much
  sense for stacked bar charts (everything is 100% of itself), but it can be useful in some scenarios.
}

@defproc[(histogram [#:x-min x-min (or/c rational? #f) #f]
                    [#:x-max x-max (or/c rational? #f) #f]
                    [#:y-min y-min (or/c rational? #f) #f]
                    [#:y-max y-max (or/c rational? #f) #f]
                    [#:color color plot-color/c (rectangle-color)]
                    [#:style style plot-brush-style/c (rectangle-style)]
                    [#:line-color line-color plot-color/c (rectangle-line-color)]
                    [#:line-width line-width (>=/c 0) (rectangle-line-width)]
                    [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
                    [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
                    [#:label label (or/c string? pict? #f) #f]
                    [#:bins bins positive-integer? 30]
                    [#:mapping local-mapping
                               (aes-containing/c #:x string?
                                                 #:y (or/c string? #f)
                                                 #:facet (or/c string? #f))
                               (aes)])
         graphite-renderer?]{
  Renders a histogram. This is not the same as a bar chart, as the x-axis must be a continuous variable.

  The argument @tt{#:bins} dictates the number of bins on the x-axis.

  The optional @tt{#:y} aesthetic will be the average of every observation in the given x-axis bin. If not
  specified, this will default to the count of the number of elements in the bin. Anecdotally, if you use
  this, you may be better off with @racket[points] or @racket[lines].
}

@defproc[(density [#:x-min x-min (or/c rational? #f) #f]
                  [#:x-max x-max (or/c rational? #f) #f]
                  [#:y-min y-min (or/c rational? #f) #f]
                  [#:y-max y-max (or/c rational? #f) #f]
                  [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                  [#:color color plot-color/c (line-color)]
                  [#:width width (>=/c 0) (line-width)]
                  [#:style style plot-pen-style/c (line-style)]
                  [#:alpha alpha (real-in 0 1) (line-alpha)]
                  [#:label label (or/c string? pict? #f) #f]
                  [#:mapping local-mapping
                             (aes-containing/c #:x string?
                                               #:facet (or/c string? #f)
                                               #:discrete-color (or/c string? #f))
                             (aes)])
         graphite-renderer?]{
  Renders estimated density for the given points. The only suppported kernel is the Gaussian, as this
  is the only supported kernel in @racketmodname[plot].
}

@section[#:tag "transforms"]{Axis Transforms}

@defstruct*[transform ([function (-> any/c any/c)]
                       [inverse (-> any/c any/c)]
                       [axis-ticks ticks?])]{
  Represents an axis transform, to be used in the @tt{#:x-transform} or @tt{#:y-transform}
  argument of @racket[graph].

  Takes a function, its inverse, and a @racket[ticks?] to transform the axis.
  For example, for a logarithmic transform, you could use:
  @racketblock[
    (transform (λ (x) (log x 10)) (λ (x) (expt 10 x))
               (log-ticks))
  ]
}

@defproc[(get-adjusted-ticks [transform transform?])
         ticks?]{
  Gets the adjusted @racket[ticks?] from the axis transform, for actual usage as @racket[plot-x-ticks]
  or @racket[plot-y-ticks]. Useful both internally and for interoperation with @racketmodname[plot].
}

@defproc[(only-ticks [ticks ticks?])
         transform?]{
  Constructs a new transform that only changes the axis ticks according to the input ticks, without
  actually manipulating the data.
}

@defthing[no-transform transform?]{
  The dummy axis transform. Does not manipulate the data.
}

@defthing[logarithmic-transform transform?]{
  A logarithmic (base 10) axis transform.
}

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
     (eval '(define organdata (df-read/csv "data/organdata.csv" #:na "NA")))
     (eval '(define midwest (df-read/csv "data/midwest.csv")))
     (eval '(define gapminder (df-read/csv "data/all_gapminder.csv")))
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
                [#:theme theme graphite-theme? theme-default]
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

  The @tt{#:x-conv} and @tt{#:y-conv} arguments, if given, perform pre-processing of the x-axis and y-axis variable
  (when said variables are not automatically determined). For example, if you wanted to place dates on the
  x-axis, this could be a function converting your preferred date format to seconds since the UNIX epoch.

  The @tt{#:x-transform} and @tt{#:y-transform} arguments, if given, take a @racket[transform?] to adjust the x and
  y axes, as well as the ticks. For example, if you wanted to place a logarithmic transform on the x-axis, you
  could specify @racket[logarithmic-transform]. Transforms are applied @italic{after} the respective @tt{#:x-conv}
  or @racket{#:y-conv} function, if present.

  When given, the @tt{#:x-min} (etc.) arguments determine the bounds of the plot, but not the bounds of the
  individual renderers. For this, the data should be trimmed before being passed in.

  The aesthetic @tt{#:facet}, specified in the @tt{#:mapping} argument, dictates whether to facet on a
  single categorical variable. If this is selected, Graphite will split the plot into subplots based on that
  variable, into a grid. This aesthetic does nothing if not applied globally.

  The optional @tt{#:facet-wrap} argument dictates how many columns should be drawn before wrapping to a new
  line. By default, this is the square root of the number of observations in the @tt{#:facet} variable, creating
  a grid.
}

@defproc[(save-pict [pict pict?]
                    [path path-string?])
         exact-nonnegative-integer?]{
  Saves a @racket[pict?] to disk, at the given path. Supports saving as PNG, PDF, or SVG, depending on the file
  extension.
}

@section[#:tag "aesthetics"]{Aesthetic Mappings}

Aesthetic mappings are used to map a given "aesthetic" (such as the x-axis, y-axis, or color) to a variable. When
doing this, the given aesthetic will be "split" on that variable. Every renderer, as well as @racket[graph],
takes an aesthetic mapping using the @tt{#:mapping} keyword.

@defproc[(aes [#:<key> value any/c] ...) aes?]{
  Creates an aesthetic mapping, with each @tt{#:<key>} being mapped to each value.

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

@defproc[(boxplot [#:invert? invert? boolean? #f]
                  [#:iqr-scale iqr-scale real? 1.5]
                  [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
                  [#:box-color box-color plot-color/c (rectangle-color)]
                  [#:box-style box-style plot-brush-style/c (rectangle-style)]
                  [#:box-line-color box-line-color plot-color/c (rectangle-line-color)]
                  [#:box-line-width box-line-width (>=/c 0) (rectangle-line-width)]
                  [#:box-line-style box-line-style plot-pen-style/c (rectangle-line-style)]
                  [#:box-alpha box-alpha (real-in 0 1) (rectangle-alpha)]
                  [#:show-outliers? show-outliers? boolean? #t]
                  [#:outlier-color outlier-color plot-color/c (point-color)]
                  [#:outlier-sym outlier-sym point-sym/c (point-sym)]
                  [#:outlier-fill-color outlier-fill-color (or/c plot-color/c 'auto) 'auto]
                  [#:outlier-size outlier-size (>=/c 0) (point-size)]
                  [#:outlier-line-width outlier-line-width (>=/c 0) (point-line-width)]
                  [#:outlier-alpha outlier-alpha (real-in 0 1) (point-alpha)]
                  [#:show-whiskers? show-whiskers? boolean? #t]
                  [#:whiskers-color whiskers-color plot-color/c (line-color)]
                  [#:whiskers-width whiskers-width (>=/c 0) (line-width)]
                  [#:whiskers-style whiskers-style plot-pen-style/c (line-style)]
                  [#:whiskers-alpha whiskers-alpha (real-in 0 1) (line-alpha)]
                  [#:show-median? show-median? boolean? #t]
                  [#:median-color median-color plot-color/c (line-color)]
                  [#:median-width median-width (>=/c 0) (line-width)]
                  [#:median-style median-style plot-pen-style/c (line-style)]
                  [#:median-alpha median-alpha (real-in 0 1) (line-alpha)]
                  [#:mapping local-mapping
                             (aes-containing/c #:x string?
                                               #:y string?
                                               #:facet (or/c string? #f))
                             (aes)])
        graphite-renderer?]{
  Renders a box-and-whisker plot.

  The optional @tt{#:invert?} argument, if true, will draw the box-and-whisker plots lengthwise (left-to-right)
  rather than top-to-bottom. This means that the axes will have to be inverted. For example:
  @examples[#:eval ev #:label #f
    (graph #:data organdata
           #:mapping (aes #:x "country" #:y "donors")
           (boxplot))
    (graph #:data organdata
           #:mapping (aes #:x "donors" #:y "country")
           (boxplot #:invert? #t))
  ]
  where the second graph is wildly preferable.

  The optional @tt{#:iqr-scale} argument is the multiplier used to determine the lower and upper limits (IQR)
  and which points are considered arguments. These limits are calculated as @racket[(* iqr-scale (- Q3 Q1))],
  where Q1 and Q3 are (respectively) the first and third quantile of the data.
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

@section[#:tag "themes"]{Theming}

@defstruct*[graphite-theme ([foreground plot-color/c]
                            [foreground-alpha (real-in 0 1)]
                            [background plot-color/c]
                            [background-alpha (real-in 0 1)]
                            [font-size (>=/c 0)]
                            [font-face (or/c string? #f)]
                            [font-family font-family/c]
                            [pen-color-map (or/c symbol? #f)]
                            [brush-color-map (or/c symbol? #f)])]{
  Represents a theme, to be passed by the @racket[#:theme] argument to @racket[graph].

  These fields correspond to, respectively, the @racketmodname[plot] parameters @racket[plot-foreground],
  @racket[plot-foreground-alpha], @racket[plot-background], @racket[plot-background-alpha],
  @racket[plot-font-size], @racket[plot-font-face], @racket[plot-font-family],
  @racket[plot-pen-color-map], and @racket[plot-brush-color-map]. They correspond to the same
  behaviors in @racket[plot].

  The @racket[pen-color-map] field corresponds to the color-map used to draw points and lines,
  such as that in @racket[points] or @racket[lines]. The @racket[brush-color-map] field corresponds
  to the color-map used to draw rectangles and other large fields, such as @racket[histogram] and
  @racket[bar].
}

@defproc[(make-graphite-theme [#:fg fg plot-color/c (plot-foreground)]
                              [#:fg-alpha fg-alpha (real-in 0 1) (plot-foreground-alpha)]
                              [#:bg bg plot-color/c (plot-background)]
                              [#:bg-alpha bg-alpha (real-in 0 1) (plot-background-alpha)]
                              [#:font-size font-size (>=/c 0) (plot-font-size)]
                              [#:font-face font-face (or/c symbol? #f) (plot-font-face)]
                              [#:font-family font-family font-family/c (plot-font-family)]
                              [#:color-map color-map (or/c symbol? #f) (plot-pen-color-map)]
                              [#:brush-color-map brush-color-map (or/c symbol? #f) (plot-brush-color-map)])
         graphite-theme?]{
  Constructs a @racket[graphite-theme?], but with keyword-arguments for readability, and with fields
  defaulting to their respective plot parameters.
}

@defproc[(theme-override [theme graphite-theme?]
                         [#:fg fg plot-color/c (graphite-theme-foreground theme)]
                         [#:fg-alpha fg-alpha (real-in 0 1) (graphite-theme-foreground-alpha theme)]
                         [#:bg bg plot-color/c (graphite-theme-background theme)]
                         [#:bg-alpha bg-alpha (real-in 0 1) (graphite-theme-background-alpha theme)]
                         [#:font-size font-size (>=/c 0) (graphite-theme-font-size theme)]
                         [#:font-face font-face (>=/c 0) (graphite-theme-font-face theme)]
                         [#:font-family font-family font-family/c (graphite-theme-font-family theme)]
                         [#:color-map color-map (or/c symbol? #f) (graphite-theme-pen-color-map theme)]
                         [#:brush-color-map brush-color-map (or/c symbol? #f)
                                            (graphite-theme-brush-color-map theme)])
         graphite-theme?]{
  Constructs a @racket[graphite-theme?], but overriding fields from an existing theme. For example, if you
  wanted to use Comic Sans with the default theme, you could use the theme:
  @racketblock[
    (theme-override theme-default #:font-face "Comic Sans MS")
  ]
}

@defthing[theme-default graphite-theme?]{
  The default theme. Defined as:
  @racketblock[
    (make-graphite-theme #:fg "black" #:fg-alpha 1
                         #:bg "white" #:bg-alpha 1
                         #:font-size 11 #:font-family 'swiss
                         #:color-map 'set1 #:brush-color-map 'pastel1)
  ]

  Uses a sans-serif font, white background, black text, and the @racket['set1] color-map and @racket['pastel1]
  brush color map. See the documentation for @racket[plot-pen-color-map] for the colors of this theme.
}

@defthing[theme-continuous graphite-theme?]{
  A theme for plotting continuous data. Defined as the default theme, except with the color-map set to
  @racket['tol-is] from the @tt{colormaps} package.

  @examples[#:eval ev
    (df-add-derived! gapminder "log-pop" '("pop") (λ (x) (log (first x) 10)))
    (graph #:data gapminder
           #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
           #:x-transform logarithmic-transform
           #:legend-anchor 'no-legend
           #:theme theme-continuous
           (points #:mapping (aes #:continuous-color "log-pop")))
  ]
}

@defproc[(theme->alist [theme graphite-theme?]) (listof (cons/c parameter? any/c))]{
  Converts a @racket[graphite-theme?] to an association list of @racketmodname[plot] parameters to
  the values specified by the theme.
}

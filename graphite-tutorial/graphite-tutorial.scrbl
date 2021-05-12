#lang scribble/manual

@(require scribble/example
          (for-label racket data-frame graphite))

@(define ev
   (let ([eval (make-base-eval)])
     (eval '(begin
              (require data-frame
                       graphite
                       plot/utils
                       threading)))
     eval))

@title{Graphite: a guided tour}

Graphite is a library for data visualization, designed to create common types of plots with as little
effort as possible, and allowing for easy manipulation of the plot around the data without having to
change the structure of the data itself. The interface is loosely modeled
around the tidyverse's ggplot2, so if you're familiar with it, you should feel at home.

To facilitate this, we depend on the @racket[data-frame] library as the primary data structure.
Once your data is read into a data frame, Graphite is able to work with it.

@section{Deciding what library to use}

Graphite is implemented @italic{on top} of @racket[plot], and in no way serves as a replacement to it, instead
serving as a complement. As a consequence, Graphite's functionality is in no way a strict superset of
@racket[plot].

If your data visualization satisfies some of the following criteria, Graphite would be a good fit:
@itemlist[
  @item{@bold{Your data is comprised of discrete points.} Graphite requires all data to be read into a
        @racket[data-frame] before creating any visualization. This means that if your plot consists of
        continuous data (e.g. a function), Graphite is unlikely to fit your needs.}
  @item{@bold{Your plot is intended for use as an image.} Graphite exports all plots as a @racket[pict].
        For various reasons, it does not support the interactivity that @racket[plot] snips provide.
        Graphite's generated plots work great in Scribble, or embedded into any other document by saving
        the file using @racket[save-pict].}
  @item{@bold{Your intended plot is 2D.} Graphite does not support 3D plots, and it probably never will.}
  @item{@bold{Your data is tidy, and consists of the data you actually want to show.} Graphite assumes that
        your data is in "tidy" form, with each variable being a column and each observation being a row. Data
        wrangling is outside of the scope of Graphite's functionality.}
  @item{@bold{You don't know what data visualization method you want to use.} Graphite's main goal is to
        prevent significant structural changes when, for example, switching from a scatter plot to a
        histogram.}
]

If your data is untidy, it will require further processing before being read into a @racket[data-frame].
Anecdotally, this is not particularly easy in Racket. All data in this tutorial is already tidy.

If your data is primarily continuous, needs to be interactive, or needs to be 3D, @racket[plot] is likely to
be a better fit.

@section{Gapminder}

All data visualization starts with data to visualize, and we begin with excerpts of data from
@hyperlink["www.gapminder.org/data/" "Gapminder"]: more specifically, we begin with a CSV dump of the
data in the @hyperlink["https://cran.r-project.org/web/packages/gapminder/README.html" "Gapminder library
for R"]. This data is already tidy and in the format we want, so we merely read it in as a CSV using
@racket[df-read/csv] from the data-frame library:

@examples[#:eval ev #:label #f
  (define gapminder (df-read/csv "data/all_gapminder.csv"))
]

We can take a quick look at our data using the @racket[df-describe] function, to determine what data we
actually have:
@examples[#:eval ev #:label #f
  (df-describe gapminder)
]

So, we know that we have GDP per capita and life-expectancy in our dataset. Let's say we wanted to make a
scatterplot of these two variables versus each other. Using Graphite, that would look like this:
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         (points))
]

Let's break down this code. The main form is @racket[graph], which takes a number of keyword arguments. The
@tt{#:data} keyword argument specifies the data-frame that we want to plot.

The @tt{#:mapping} keyword argument specifies our @racket[aes] (standing for @italic{aesthetics}), which
dictates how we actually want the dat to be shown on the plot. In this case, our mapping states that we want
to map the x-axis to the variable @tt{gdpPercap}, and the y-axis to the variable @tt{lifeExp}.

Finally, the rest of our arguments dictate our renderers. In this case, the @racket[points] renderer states
that we want each data point to be drawn as a single point.

This plot is fine, but it's rather unenlightening --- we have a lot of blank space towards the bottom. This
can be remedied by adding a logarithmic transform on the x-axis. We could specify this manually, but Graphite
already has a log transform predefined:
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         (points))
]

The @tt{#:x-transform} keyword argument specifies an @racket[invertible-function] that dictates an axis
transform. In this case, we use the @racket[logarithmic-transform] function, which is already defined.

This plot is starting to look nicer, but it's pretty unenlightening. We don't know anything about each country
or how they're stratified, we can't figure out how many countries are present at any given point, we can't
extrapolate a meaningful relationship aside from "probably linear-ish", and we haven't labeled our axes. We
can start by adding labels, and setting the alpha value of the renderer to see where more countries are present:
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         (points #:mapping (aes #:alpha 0.4)))
]

All we've done here is added labels and titles via their eponymous keyword arguments, and added an @racket[aes]
to the renderer @racket[points]'s mapping. Each renderer, including @racket[points], can take its own mapping,
which overrides the global mapping set in the @racket[graph] form.

We can then start thinking about relationships between all the data-points. Let's say we wanted to add a linear
fit to our plot. Then, we can use the @racket[fit] renderer:
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         (points #:mapping (aes #:alpha 0.4))
         (fit #:mapping (aes #:width 3)))
]

@racket[fit] defaults to a linear fit (of degree 1), but you can instead do a fit using a higher-degree
polynomial with the optional @tt{#:degree} argument:
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         (points #:mapping (aes #:alpha 0.4))
         (fit #:degree 3 #:mapping (aes #:width 3)))
]
but this is ill-advised for the relationship we see here.

Finally, let's try and extrapolate different relationships for each continent. We can stratify the points alone
by using the aesthetic @tt{#:discrete-color} to @racket[points], which lets us pick a categorical variable
to change the color on, in this case @tt{"continent"}:
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         (points #:mapping (aes #:alpha 0.4 #:discrete-color "continent"))
         (fit #:mapping (aes #:width 3)))
]

Now we're seeing some notable differences from where we've started! We made a scatter plot, transformed its
axes, labeled it, and added aesthetics to make it more readable.

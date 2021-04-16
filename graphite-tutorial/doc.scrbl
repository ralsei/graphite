#lang scribble/manual

@(require scribble/example
          (for-label racket data-frame plot/pict graphite))

@(define ev
   (let ([eval (make-base-eval)])
     (eval '(begin
              (require data-frame
                       graphite
                       plot/pict
                       plot/utils
                       threading)))
     eval))

@title{Graphite: a mildly opinionated data visualization library}

Graphite is a thin layer over the existing @racket[plot] library, designed to create common types
of plots with as little effort as possible, and allowing for easy manipulation of the plot around
the data without having to change the structure of the data itself. The interface is loosely modeled
around the tidyverse's ggplot2, so if you're familiar with it, you should feel at home.

To facilitate this, we depend on the @racket[data-frame] library as the primary data structure.
Once your data is read into a data frame, Graphite is able to work with it.

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

So, we know that we have GDP per capita, and we know that we have life-expectancy in our dataset. Let's
say we wanted to make a scatterplot of these two variables versus each other. Using Graphite, that would
look like this:
@examples[#:eval ev #:label #f
  (pplot #:data gapminder
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         (ppoints))
]

Let's break down this code. The main form is @racket[pplot], which takes a number of keyword arguments. The
@tt{#:data} keyword argument specifies the data-frame that we want to plot.

The @tt{#:mapping} keyword argument specifies our @racket[aes] (standing for @italic{aesthetics}), which
dictates how we actually want the dat to be shown on the plot. In this case, our mapping states that we want
to map the x-axis to the variable @tt{gdpPercap}, and the y-axis to the variable @tt{lifeExp}.

Finally, the rest of our arguments dictate our renderers. In this case, the @racket[ppoints] renderer states
that we want each data point to be drawn as a single point.

This plot is fine, but it's rather unenlightening --- we have a lot of blank space towards the bottom. This
can be remedied by adding a logarithmic transform on the x-axis. We could specify this manually, but Graphite
already has a log transform predefined:
@examples[#:eval ev #:label #f
  (pplot #:data gapminder
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         #:x-ticks (log-ticks #:scientific? #f)
         (ppoints))
]

The @tt{#:x-transform} keyword argument specifies an @racket[invertible-function] that dictates an axis
transform. In this case, we use the @racket[logarithmic-transform] function, which is already defined.
Additionally, the @tt{#:x-ticks} argument specifies what ticks we want to use on the x-axis. Since we used
a log transform, it makes sense we want to use @racket[log-ticks]. Finally, we disable scientific notation,
as we're working with currency, so it doesn't make much sense.

This plot is starting to look nicer, but it's pretty unenlightening. We don't know anything about each country
or how they're stratified, we can't figure out how many countries are present at any given point, we can't
extrapolate a meaningful relationship aside from "probably linear-ish", and we haven't labeled our axes. We
can start by adding labels, and setting the alpha value of the renderer to see where more countries are present:
@examples[#:eval ev #:label #f
  (pplot #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         #:x-ticks (log-ticks #:scientific? #f)
         (ppoints #:mapping (aes #:alpha 0.4)))
]

All we've done here is added labels and titles via their eponymous keyword arguments, and added an @racket[aes]
to the renderer @racket[ppoints]'s mapping. Each renderer, including @racket[ppoints], can take its own mapping,
which overrides the global mapping set in the @racket[pplot] form.

We can then start thinking about relationships between all the data-points. Let's say we wanted to add a linear
fit to our plot. Then, we can use the @racket[fit] renderer:
@examples[#:eval ev #:label #f
  (pplot #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         #:x-ticks (log-ticks #:scientific? #f)
         (ppoints #:mapping (aes #:alpha 0.4))
         (fit #:method 'linear #:mapping (aes #:width 3)))
]

In this case, we have a special argument to @racket[fit], @tt{#:method}, that specifies how we want the fit
line to be constructed. If we wanted, for example, an (ill-advised) logarithmic fit, we could use the @tt{'log}
method.

Finally, let's try and extrapolate different relationships for each continent. We can stratify the points alone
by using the aesthetic @tt{#:discrete-color} to @racket[ppoints], which lets us pick a categorical variable
to change the color on, in this case @tt{"continent"}:
@examples[#:eval ev #:label #f
  (pplot #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         #:x-ticks (log-ticks #:scientific? #f)
         (ppoints #:mapping (aes #:alpha 0.4 #:discrete-color "continent"))
         (fit #:method 'linear #:mapping (aes #:width 3)))
]

Now we're seeing some notable differences from where we've started! We made a scatter plot, transformed its
axes, labeled it, and added aesthetics to make it more readable.

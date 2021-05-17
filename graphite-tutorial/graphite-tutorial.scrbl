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

This tutorial is modeled around Kieran Healy's work @hyperlink["https://www.socviz.co"
"Data Visualization: A practical introduction"], which uses ggplot2 and R rather than Graphite and Racket.

@table-of-contents[]

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

@section{Bar charts}

For this section, we'll be using a CSV dump of the 2016 GSS (General Social Survey) from its respective R
library, a dataset that sociologists continually manage to squeeze more and more insights out of. More
importantly, the Gapminder dataset from the previous section has a lot of continuous variables (such as
GDP per capita and life expectancy, which we worked with), but no categorical variables. The GSS has a wide
variety of categorical variables to work with, making it ideal for making bar charts and histograms.

Similarly to last time, we load it up and take a gander:
@examples[#:eval ev #:label #f
  (define gss (df-read/csv "data/gss_sm.csv"))
  (df-describe gss)
]

Clearly, we have a lot of data to work with here, but a lot of it is categorical, so @racket[df-describe]'s
summary statistics aren't particularly useful.

We start off by taking a look at the variable @tt{religion}, which contains a condensed version of the religions
in the GSS. (The variable @tt{relig} is more descriptive, but has too many categories for simple examples.)
We use the @racket[bar] renderer, with no arguments, to take a look at the count:
@examples[#:eval ev #:label #f
  (graph #:data gss
         #:title "Religious preferences, GSS 2016"
         #:mapping (aes #:x "religion")
         (bar))
]

Unfortunately, the size of the text and the default size of our visualization don't play nice. We can extend
the width and height of the result with their eponymous keywords:
@examples[#:eval ev #:label #f
  (graph #:data gss
         #:title "Religious preferences, GSS 2016"
         #:mapping (aes #:x "religion")
         #:width 600 #:height 400
         (bar))
]

Much easier to read. Let's say that we wanted to, instead, look at the @italic{proportion} of each religion
among the whole, rather than its individual count. We can specify this with the @tt{#:mode} argument of
@racket[bar], which can either be @tt{'count} or @tt{'prop}, with @tt{'count} being the default behavior
we saw before.
@examples[#:eval ev #:label #f
  (graph #:data gss
         #:title "Religious preferences, GSS 2016"
         #:mapping (aes #:x "religion")
         #:width 600 #:height 400
         (bar #:mode 'prop))
]

With the y-axis representing proportions from 0 to 1, we now have a good idea of what's going on here. Similarly
to the last example with Gapminder, let's say that we wanted to split on each region, cross-classifying between
the categorical variables of @tt{religion} and @tt{bigregion} (Northeast/Midwest/South/West, in the US). To
accomplish this, we can make the x-axis region, and then "dodge" on the variable @tt{religion} -- effectively,
making each individual region its own bar chart. To do this, we use the aesthetic @tt{#:group}:
@examples[#:eval ev #:label #f
  (graph #:data gss
         #:title "Religious preferences among regions, GSS 2016"
         #:mapping (aes #:x "bigregion" #:group "religion")
         #:width 600 #:height 400
         (bar #:mode 'prop))
]

But this is pretty difficult to read as well! There's a lot of bars in each section, and you're forced to consult
the legend for the bar colors for each one. To mitigate this, we can introduce another concept...

@section{Faceting}

@;; TODO: fix w/h, add wrapping would make this section so much nicer...

In both the Gapminder and GSS examples, we ended with a whole bunch of information crammed into a single
visualization. Let's say we instead wanted to make things more clear. In this case, we can add a @italic{facet}
to our visualization, which creates multiple plots in different panels.

Faceting is a global aesthetic (used by @racket[graph], and not individual renderers), dictated by the keyword
@tt{#:facet}. To use it, we specify it to be a variable, and @racket[graph] will split the plot up for us.
Take the previous GSS example: let's say we wanted to instead facet on @tt{bigregion}, and then have each subplot
represent religious preferences in that region. In that case:
@examples[#:eval ev #:label #f
  (graph #:data gss
         #:mapping (aes #:x "religion" #:facet "bigregion")
         (bar #:mode 'prop))
]

Now we've managed to split up our visualization into seperate charts for each region. But, we're now facing
the issue that they're all in a straight line, and our text isn't displaying the way we want it to on the x-axis.

This is where the tutorial currently ends, as this hinges on currently unimplemented functionality. More to come!

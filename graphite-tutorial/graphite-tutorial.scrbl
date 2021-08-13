#lang scribble/manual

@(require scribble/example
          (for-label (except-in racket rename)
                     data-frame
                     graphite
                     sawzall
                     threading))

@(define ev
   (let ([eval (make-base-eval)])
     (eval '(begin
              (require data-frame
                       graphite
                       plot/utils
                       racket/string
                       racket/vector
                       threading
                       sawzall)))
     eval))

@title{Graphite: a guided tour}
@author{@(author+email "Hazel Levine" "hazel@knightsofthelambdacalcul.us")}

Graphite is a library for data visualization, designed to create common types of plots with as little
effort as possible, and allowing for easy manipulation of the plot around the data without having to
change the structure of the data itself. The interface is loosely modeled
around the tidyverse's ggplot2, so if you're familiar with it, you should feel at home.

This tutorial is modeled around Kieran Healy's work @hyperlink["https://www.socviz.co"
"Data Visualization: A practical introduction"], which uses ggplot2 and R rather than Graphite and Racket.

@table-of-contents[]

@section{Deciding what library to use}

Graphite is implemented @italic{on top} of @racketmodname[plot], and in no way serves as a replacement to it,
instead serving as a complement. As a consequence, Graphite's functionality is in no way a strict superset of
@racketmodname[plot].

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
  @item{@bold{You don't know what data visualization method you want to use.} Graphite's main goal is to
        prevent significant structural changes when, for example, switching from a scatter plot to a
        histogram.}
]

If your data is untidy, it will require further processing before being read into a @racket[data-frame].
This will be covered later in the tutorial, and requires the Sawzall library
(docs at @other-doc['(lib "sawzall-doc/sawzall.scrbl")]), specifically designed to complement Graphite.

If your data is primarily continuous, needs to be interactive, or needs to be 3D, @racket[plot] is likely to
be a better fit.

@section{Key forms}

Graphite has a number of important forms that are effectively required for anything to come out of it.

First off, in order to plot data, we need to read data in. To do this, Graphite uses the
@racketmodname[data-frame] library. Usually, to read in data from a CSV, you use @racket[df-read/csv].
Various other formats are supported by @racketmodname[data-frame], which you can find by consulting its
documentation.

The main form of Graphite-generated plots is the function @racket[graph]. It takes two mandatory keywords,
@racket[#:data] and @racket[#:mapping], which correspond to (respectively) the @racket[data-frame?] to read
your data from and the @racket[aes?] to retrieve global aesthetics from (more on that later). Finally, it takes
any amount of renderers, such as @racket[points] and @racket[lines], as positional arguments. As an example,
a usual call to @racket[graph] would look like:
@racketblock[
  (graph #:data some-data
         #:mapping (aes #:some-key some-value)
         #:title "a title"
         (renderer-1)
         (renderer-2))
]

Aesthetic mappings, created with the @racket[aes] form, creates a set of key-value mappings, with keys specified
by keywords. Each key represents an "aesthetic" to map a variable to, and each value represents a variable in the
data to map it to. If a value is not present and not mandatory, it is read as @racket[#f]. These can correspond
to positional aesthetics (the x-axis with @racket[#:x], and the y-axis with @racket[#:y]), colorings
(such as @racket[#:discrete-color] to @racket[points]), or another required variable
(such as @racket[#:perc-error] to @racket[error-bars]). The most important thing is that aesthetic mappings only
correspond to mapping an aesthetic to a @italic{variable}: if you want to set it as a constant, you likely want
a regular keyword argument.

@racket[graph] takes a global aesthetic mapping, and each renderer takes its own aesthetic mapping as well,
all with the @racket[#:mapping] keyword. When a renderer is called, it inherits aesthetics from both the global
mapping passed to @racket[graph] and the local mapping passed to it. In addition, the local mapping overrides
the global mapping's settings. So, for example, in this code:
@racketblock[
  (graph #:data some-data
         #:mapping (aes #:foo "bar" #:baz "quux")
         (renderer-1 #:mapping (aes #:baz "waldo" #:fruit "apple")))
]
the aesthetic mapping that is actually used in @racket[renderer-1] would be
@racket[(aes #:foo "bar" #:baz "waldo" #:fruit "apple")], inheriting @racket[#:foo] from the global mapping
and overriding @racket[#:baz].

@section{Gapminder}

All data visualization starts with data to visualize, and we begin with excerpts of data from
@hyperlink["https://www.gapminder.org/data/" "Gapminder"]: more specifically, we begin with a CSV dump of the
data in the @hyperlink["https://cran.r-project.org/web/packages/gapminder/README.html" "Gapminder library
for R"]. This data is already tidy and in the format we want, so we merely read it in as a CSV using
@racket[df-read/csv] from the data-frame library:

@examples[#:eval ev #:label #f
  (define gapminder (df-read/csv "data/all_gapminder.csv"))
]

We can take a quick look at our data using the @racket[show] form from the @racketmodname[sawzall] library
(more on that later!), to determine what data we actually have:
@examples[#:eval ev #:label #f
  (show gapminder)
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
dictates how we actually want the data to be shown on the plot. In this case, our mapping states that we want
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

The @tt{#:x-transform} keyword argument specifies a @racket[transform?], which combines a @racketmodname[plot]
transform and ticks. In this case, we use the @racket[logarithmic-transform] function, which is already defined.

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
         (points #:alpha 0.4))
]

All we've done here is added labels and titles via their eponymous keyword arguments, and added a keyword to
the renderer @racket[points].

We can then start thinking about relationships between all the data-points. Let's say we wanted to add a linear
fit to our plot. Then, we can use the @racket[fit] renderer:
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         (points #:alpha 0.4)
         (fit #:width 3))
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
         (points #:alpha 0.4)
         (fit #:degree 3 #:width 3))
]
but this is ill-advised for the relationship we see here.

Finally, let's try and extrapolate different relationships for each continent. We can stratify the points alone
by using the aesthetic @tt{#:discrete-color} to @racket[points], which lets us pick a categorical variable
to change the color on, in this case @tt{"continent"}. Each renderer also takes its own mapping, which can be
used to map some aesthetic to a variable.
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
         #:x-transform logarithmic-transform
         (points #:alpha 0.4 #:mapping (aes #:discrete-color "continent"))
         (fit #:width 3))
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
  (show gss)
]

Clearly, we have a lot of data to work with here, but a lot of it is categorical -- meaning we can make some
bar charts!

We start off by taking a look at the variable @tt{religion}, which contains a condensed version of the religions
in the GSS. (The variable @tt{relig} is more descriptive, but has too many categories for simple examples.)
We use the @racket[bar] renderer, with no arguments, to take a look at the count:
@examples[#:eval ev #:label #f
  (graph #:data gss
         #:title "Religious preferences, GSS 2016"
         #:mapping (aes #:x "religion")
         (bar))
]

Let's say that we wanted to, instead, look at the @italic{proportion} of each religion
among the whole, rather than its individual count. We can specify this with the @tt{#:mode} argument of
@racket[bar], which can either be @tt{'count} or @tt{'prop}, with @tt{'count} being the default behavior
we saw before.
@examples[#:eval ev #:label #f
  (graph #:data gss
         #:title "Religious preferences, GSS 2016"
         #:mapping (aes #:x "religion")
         (bar #:mode 'prop))
]

With the y-axis representing proportions from 0 to 1, we now have a good idea of what's going on here. Similarly
to the last example with Gapminder, let's say that we wanted to split on each region, cross-classifying between
the categorical variables of @tt{religion} and @tt{bigregion} (Northeast/Midwest/South/West, in the US). To
accomplish this, we can make the x-axis region, and then "dodge" on the variable @tt{religion} -- effectively,
making each individual region its own bar chart. To do this, we use the aesthetic @tt{#:group}, and adjust
the plot size:
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

In both the Gapminder and GSS examples, we ended with a whole bunch of information crammed into a single
visualization. Let's say we instead wanted to make things more clear. In this case, we can add a @italic{facet}
to our visualization, which creates multiple plots in different panels.

Faceting is a global aesthetic (used by @racket[graph], and not individual renderers), dictated by the keyword
@tt{#:facet}. To use it, we specify it to be a variable, and @racket[graph] will split the plot up for us.
Take the previous GSS example: let's say we wanted to instead facet on @tt{bigregion}, and then have each subplot
represent religious preferences in that region. In that case:
@examples[#:eval ev #:label #f
  (parameterize ([plot-x-tick-label-angle 30]
                 [plot-x-tick-label-anchor 'top-right])
    (graph #:data gss
           #:mapping (aes #:x "religion" #:facet "bigregion")
           #:width 700 #:height 700
           (bar #:mode 'prop)))
]

Now we've managed to split up our visualization into seperate charts for each region. In addition, this
demonstrates that missing functionality in Graphite can be added with the regular @racketmodname[plot] parameters,
in this case to rotate the labels.

We can also do this for the Gapminder plot, by faceting by continent:
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:title "GDP per capita vs life expectancy"
         #:x-label "GDP per capita (USD)"
         #:y-label "Life expectancy (years)"
         #:mapping (aes #:x "gdpPercap" #:y "lifeExp" #:facet "continent")
         #:x-transform logarithmic-transform
         #:height 700 #:width 1000
         (points #:alpha 0.4)
         (fit #:width 3))
]

Note that we have to pay attention to the variable we're faceting on, and make sure that that variable makes sense
to facet on. For example, if we were to facet on @racket["country"], we would get something close to 150 frames!

Let's work up a more complex plot, with faceting in mind. Say we want to take Gapminder, and get data about GDP per
capita over time. Because this is a time series, it makes sense to use the @racket[lines] renderer. Here's an
initial attempt:
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:mapping (aes #:x "year" #:y "gdpPercap")
         (lines))
]

Whoa! So the problem here is that Graphite doesn't know how to handle all the data it's being fed. Namely, we have
a bunch of different data-points split up by continent. This information is @italic{correct}: but we haven't handled
it correctly with our renderers.

One way to approach this is to instead get continent-level data, and make a fit for each of them. To first get data
by continent, we can use a facet, like before. We map color to country, and hide the legend (since there are too many).
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:mapping (aes #:x "year" #:y "gdpPercap" #:facet "continent")
         #:legend-anchor 'no-legend
         #:height 700 #:width 1000
         (lines #:mapping (aes #:discrete-color "country")))
]

This is @italic{better}, but we note that measurements in Asia are dwarfing that of poorer countries. To help offset this,
we can add a log transform on the y-axis. Also, all these colors suck, so we override it with a global gray, but keep the
coloring aesthetic so we retain our grouping.

@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:mapping (aes #:x "year" #:y "gdpPercap" #:facet "continent")
         #:y-transform logarithmic-transform
         #:legend-anchor 'no-legend
         #:height 700 #:width 1000
         (lines #:color "gray" #:mapping (aes #:discrete-color "country")))
]

Finally, we can add a fit line, so we can know general trends on each continent. We use the nicer LOESS estimation method,
since some of this data seems non-linear.
@examples[#:eval ev #:label #f
  (graph #:data gapminder
         #:mapping (aes #:x "year" #:y "gdpPercap" #:facet "continent")
         #:y-transform logarithmic-transform
         #:legend-anchor 'no-legend
         #:height 700 #:width 1000
         (lines #:color "gray" #:mapping (aes #:discrete-color "country"))
         (fit #:width 3 #:method 'loess))
]

and there we have it!

@section{Data wrangling, 101}

Graphite does some of the work for us with regards to data processing. Note the @racket['count] and @racket['prop] modes
in the earlier bar charts: we didn't have to handle that. But Graphite is not a data wrangling library, and oftentimes
it makes more sense to process our data first.

For example, in the last example, we got GDP per capita summary statistics for each continent. But what if we want the
@italic{average, global} GDP per capita over time? That's too complex of a transformation for Graphite to do for us.

For this purpose, we need the Sawzall library, whose documentation is available at
@other-doc['(lib "sawzall-doc/sawzall.scrbl")]. This library is designed to take in data-frames, and produce new ones
with some transformation applied -- with operations chaning together using the @racketmodname[threading] library.

Let's say we want to perform the above transformation on the Gapminder dataset. Effectively, what we want to do is:
@itemlist[
  @item{Take all the data in each year, ignoring country,}
  @item{average the GDP per capita within each year,}
  @item{then collect the results of that sum into a new data-frame.}
]

This translates naturally to the following Sawzall pipeline:
@examples[#:eval ev #:label #f
  (define (sum vec) (for/sum ([v (in-vector vec)]) v))
  (define (avg vec) (/ (sum vec) (vector-length vec)))

  (~> gapminder
      (group-with "year")
      (aggregate [avgGdpPercap (gdpPercap) (avg gdpPercap)])
      show)
]

Let's break down this code.
@itemlist[
  @item{The @racket[~>] operator is effectively "spicy function composition"; @racket[(~> h g f x)] translates at
        compile-time to @racket[(f (g (h x)))]. We use it here to express the idea of "do-this-then-that".}
  @item{@racket[(group-with "year")] takes @racket[gapminder], and groups it with respect to the variable @racket["year"].
        This tells sequential operations that we want to treat each different possibility of year seperately.}
  @item{@racket[(aggregate [avgGdpPercap (gdpPercap) (avg gdpPercap)])] aggregates each group into a single value.
        @racket[avgGdpPercap] tells us what the new column name should be, @racket[(gdpPercap)] tells us that we want to
        bind the variable @racket[gdpPercap] as a vector in the body, and @racket[(avg gdpPercap)] computes the average
        value of each vector.

        This is a lot to break down, but more or less it takes each year, gets all the GDP per capita values that
        correspond to it, and averages them.

        This also strips down the group structure, since we now only have one row for each year.}
  @item{@racket[show] prints out the result, and returns nothing, being the last thing in the pipeline.}
]

Instead of merely printing out the data, we can use @racket[_] to tell @racket[~>] where to put the next input, and feed
it directly into @racket[graph], printing out a time series of global GDP per capita:
@examples[#:eval ev #:label #f
  (~> gapminder
      (group-with "year")
      (aggregate [avgGdpPercap (gdpPercap) (avg gdpPercap)])
      (graph #:data _
             #:mapping (aes #:x "year" #:y "avgGdpPercap")
             (lines)))
]

This works, but isn't a very useful example, and doesn't teach us anything about how to work with NA values, et cetera.
So, for a more complex example, we'll take a look at the GSS again. We saw already that @racket[bar] can plot counts
and relative frequencies. However, oftentimes it makes more sense to get the data in the shape you want it first, and
then have Graphite focus its effort on plotting the data, rather than messing with it on-the-fly.

Say we want to plot the row-marginals of region within religion (so, within each region, what is the % of each religion).
We start from our @racket[gss] table, which has individual-level observations, and go from there. Effectively, what
we want to do is:
@itemlist[
  @item{Take our individual-level data,}
  @item{group it with respect to region, and then religion within region,}
  @item{summarize each religion into a count of respondents,}
  @item{then calculate the percentage of each religion within region.}
]

Once again, we'll build a Sawzall pipeline, but this time we'll approach it incrementally. First off, we group with
respect to the variables @racket["bigregion"] and @racket["religion"]. This means that the result is internally
different, but the result of @racket[show] does not change. It's merely a marker to tell future operations what to
do.
@examples[#:eval ev #:label #f
  (~> gss
      (group-with "bigregion" "religion")
      show)
]
Note that multiple arguments to @racket[group-with] say "group with respect to the first, then within each possibility
for the first, group with respect to the second" and so on. Successive calls to @racket[group-with] do not work.

We then want to get the number of observations within each religion. Note that @racket[aggregate] above bound a variable,
and computed something with it. Here, we just want to compute the length of the input, so we call @racket[vector-length]
on whatever variable we feel like, and turn it into a new variable @racket["count"].
@examples[#:eval ev #:label #f
  (~> gss
      (group-with "bigregion" "religion")
      (aggregate [count (bigregion) (vector-length bigregion)])
      show)
]
Note that this both removed a layer of grouping, and significantly stripped down our table. @racket[aggregate] removes all
columns except groups and the columns created (because where would it put the other observations?).

We then want to take this result, and then create new variables at the current level of grouping (with respect to region),
telling us the percent preference by region. So, what we want to do is calculate the @italic{frequency} of each religion
in each region (so the count divided by the total), and then multiply it by 100 (to turn it into a percentage). We
use Sawzall's @racket[create] operation for this:
@examples[#:eval ev #:label #f
  (define (v/ vec c) (vector-map (λ (x) (/ x c)) vec))

  (~> gss
      (group-with "bigregion" "religion")
      (aggregate [count (bigregion) (vector-length bigregion)])
      (create [frequency ([count : vector]) (v/ count (sum count))]
              [percentage (frequency) (round (* frequency 100))])
      show)
]

Let's break down this code a bit:
@itemlist[
  @item{@racket[v/] is a helper function to divide every element of a vector by a scalar.}
  @item{The first clause of this @racket[create], @racket[[frequency ([count : vector]) (v/ count (sum count))]],
        binds the variable @racket["count"] as a vector (hence the annotation), divides each element by the sum of
        the entire vector, and returns the vector.

        When every bound variable is of type @racket[vector], the body of that clause should return a vector.}
  @item{The second clause of this @racket[create], @racket[[percentage (frequency) (round (* frequency 100))]],
        binds the variable @racket["frequency"] as type @racket[element], which means that the body will map over
        each element of the vector implicitly. So, @racket[frequency] is bound to a number, and we treat it as such,
        iterating over every element of the column.

        We couldn't do this for the above, because we needed the entire vector at once in order to get the sum.}
]

Finally, we strip grouping, save our result as a bare data-frame, and do a sanity check to make sure everything sums
up to 100%. Try breaking down what this code does on your own, with the knowledge above.
@examples[#:eval ev #:label #f
  (define religion-by-region
    (~> gss
        (group-with "bigregion" "religion")
        (aggregate [count (bigregion) (vector-length bigregion)])
        (create [frequency ([count : vector]) (v/ count (sum count))]
                [percentage (frequency) (round (* frequency 100))])
        ungroup))
  (~> religion-by-region
      (group-with "bigregion")
      (aggregate [total (percentage) (sum percentage)])
      show)
]

Looks good! Some error was added by rounding, hence the 101s.

We can then feed this data into Graphite, faceting on the variable @racket["bigregion"]. We use the @racket[col]
renderer, which is like @racket[bar], except it takes its data @italic{literally} -- the value in the data correspponds
directly to the height of each bar, no computations involved.
@examples[#:eval ev #:label #f
  (graph #:data religion-by-region
         #:mapping (aes #:x "religion" #:y "percentage"
                        #:fill "religion" #:facet "bigregion")
         #:x-label "Religion" #:y-label "Percent"
         #:width 700 #:height 700
         (col))
]

Voilà.

@section{Data wrangling, 201: Wrangle harder}

This section of the tutorial is based off of
@hyperlink["https://r4ds.had.co.nz/tidy-data.html" "R for Data Science Chapter 12"], by Hadley Wickham.

@italic{The Sawzall API in this section is not finalized. There are no guarantees about backwards compatibility.}

Graphite, and the majority of Sawzall's operations, assume three things about the data:
@itemlist[
  @item{Each variable has its own column.}
  @item{Each observation has its own row.}
  @item{Each value has its own cell.}
]
This style of presentation is called "tidy data" (hence the name "tidyverse", the R software collection Graphite
and Sawzall are inspired by). Data that does not satisfy these properties is called "untidy data", and the process
of transforming it into tidy data is called "tidying".

Unfortunately, the overwhelming majority of data that you will encounter in the real world™ is untidy. Most data
is managed through spreadsheet software like Excel, and is optimized for data entry, not analysis or visualization.

Let's take a look at some untidy data. In particular, we're going to look at tuberculosis data broken down by country,
year, age, gender, and diagnosis method. This data comes from the
@hyperlink["http://www.who.int/tb/country/data/download/en/" "2014 World Health Organization Global Tuberculosis Report"].
@examples[#:eval ev #:label #f
  (define who (df-read/csv "data/who.csv" #:na "NA"))
  (df-del-series! who "")
  (show who)
  (df-series-names who)
]
We remove the empty column because it's a column of IDs, which is irrelevant for our purposes.

Anyway, wow. Ouch. This is a pretty typical dataset: it has redundant columns, weird variable codes, and missing values
abound. Chances are, it was made in spreadsheet software. So, we'll need multiple steps to try and tidy it.

Here's a breakdown of what we can deduce for now:
@itemlist[
  @item{@tt{country}, @tt{iso2}, and @tt{iso3} are variables that specify the country, the latter two being country
        codes. We don't need the latter two, then.}
  @item{@tt{year} is clearly a variable.}
  @item{Given names like @tt{newrel_f65} and @tt{new_ep_f4554}, we can reasonably infer that these are values of some
        mega-variable.}
]

So, what we want to do is take the names of each @tt{new...}, and the values of each of those columns, and turn them
into two columns: one representing the former name of the column, and one representing the value. Sawzall provides an
operation for turning column names that are actually values into a new column: @racket[pivot-longer].

To use @racket[pivot-longer], we need three things: the set of columns whose names are values and not variables
(in this case, everything starting with @tt{new}), a name of a variable to move the column names to (in this case,
@tt{key}, since we don't know what these names mean yet), and a name of a variable to move the column values
to (we know it's TB cases, so we'll call it @tt{cases}).

So, that looks something like this:
@examples[#:eval ev #:label #f
  (~> who
      (pivot-longer (starting-with "new")
                    #:names-to "key"
                    #:values-to "cases")
      show)
]

We now have significantly fewer columns, and way more rows, since we transformed all of those messy columns into two.
Note the @racket[(starting-with "new")]: this is a @italic{slice spec}, a domain-specific language for selecting
columns from data. See @racket[slice]'s documentation for more details.

We can also assume that rows where there are no entered data (so, @racket[#f] is in them) are irrelevant. To get rid of
them, we can use @racket[drop-na], which does what it says on the tin:
@examples[#:eval ev #:label #f
  (~> who
      (pivot-longer (starting-with "new")
                    #:names-to "key"
                    #:values-to "cases")
      (drop-na "cases")
      show)
]

Annoyingly, we don't understand what @tt{key} is supposed to mean. Luckily, I'm basing this tutorial off other people's
work, who have conveniently provided the information for me. So, here's the anatomy of, for example,
@racket{new_ep_f4554}:
@itemlist[
  @item{The first three letters are either @tt{new} or @tt{old}, denoting new or old cases of TB. We don't have any old
        cases in the provided data. So, in this case, we have a new case}
  @item{The next two letters denote the type of TB:
        @itemlist[
          @item{@tt{rel} stands for relapse}
          @item{@tt{ep} stands for extrapulmonary TB (this case)}
          @item{@tt{sn} stands for pulmonary TB that could not be diagnosed by a pulmonary smear (smear negative)}
          @item{@tt{sp} stands for pulmonary TB that could be diagnosed by a pulmonary smear (smear positive)}
        ]}
  @item{The sixth letter gives the sex of the patient, either @tt{m} or @tt{f} for male and female, respectively.}
  @item{The remaining numbers give an age group. For example, @tt{014} is 0-14 years old, @tt{4554} is 45-54 years
        old, and @tt{65} is 65+.}
]

Unfortunately, the cases of relapse are formatted like @tt{newrel_f65}. So, we'll use @racket[create] to replace all
instances of @racket["newrel"] with @racket["new_rel"], so we have a common baseline to split our variable on. Note
that binding the variable and making a new column with the same name shadows the old one:
@examples[#:eval ev #:label #f
  (~> who
      (pivot-longer (starting-with "new")
                    #:names-to "key"
                    #:values-to "cases")
      (drop-na "cases")
      (create [key (key) (string-replace key "newrel" "new_rel")])
      show)
]

Then, we want to split up each variable along the separator @racket["_"], pulling apart each value of @tt{key} into
three variables: @tt{new}, @tt{type}, and @tt{sex-age} (since, for example, @racket["f4554"]) is both the sex and
age variable). Sawzall provides the @racket[separate] function for this purpose:
@examples[#:eval ev #:label #f
  (~> who
      (pivot-longer (starting-with "new")
                    #:names-to "key"
                    #:values-to "cases")
      (drop-na "cases")
      (create [key (key) (string-replace key "newrel" "new_rel")])
      (separate "key" #:into '("new" "type" "sex-age") #:separator "_")
      (show ["country" "new" "type" "sex-age"]))
]
The @racket[#:into] keyword argument determines what column names to split the variable into, and
@racket[#:separator] determines a string to split each value on. Also note how @racket[show] also takes a
@racket[slice]-spec, so we can determine what columns we want to show.

We have some useless variables now. Namely, @tt{new} is just @racket["new"] repeated over and over, and @tt{iso2}
and @tt{iso3} are country codes, which are irrelevant for our purposes. We can use the @racket[slice] operator,
alongside a slice-spec that takes everything aside from these variables, to remove them from the data:
@examples[#:eval ev #:label #f
  (~> who
      (pivot-longer (starting-with "new")
                    #:names-to "key"
                    #:values-to "cases")
      (drop-na "cases")
      (create [key (key) (string-replace key "newrel" "new_rel")])
      (separate "key" #:into '("new" "type" "sex-age") #:separator "_")
      (slice (not ["new" "iso2" "iso3"]))
      show)
]

Finally, we can seperate the @tt{sex-age} variable into two variables, @tt{sex} and @tt{age}, with the
@racket[#:separator] @racket[1] -- we can specify a number, in which case the variable will be split at this
character index.
@examples[#:eval ev #:label #f
  (~> who
      (pivot-longer (starting-with "new")
                    #:names-to "key"
                    #:values-to "cases")
      (drop-na "cases")
      (create [key (key) (string-replace key "newrel" "new_rel")])
      (separate "key" #:into '("new" "type" "sex-age") #:separator "_")
      (slice (not ["new" "iso2" "iso3"]))
      (separate "sex-age" #:into '("sex" "age") #:separator 1)
      show)
]

Finally, we have some tidy data! Let's save it:
@examples[#:eval ev #:label #f
  (define tidy-who
    (~> who
        (pivot-longer (starting-with "new")
                      #:names-to "key"
                      #:values-to "cases")
        (drop-na "cases")
        (create [key (key) (string-replace key "newrel" "new_rel")])
        (separate "key" #:into '("new" "type" "sex-age") #:separator "_")
        (slice (not ["new" "iso2" "iso3"]))
        (separate "sex-age" #:into '("sex" "age") #:separator 1)))
]

Now that we finally have some tidy data, we can make some nice looking plots. Let's turn this into
a plot of TB cases in Afghanistan. We want to use @racket[where] in order to "filter" down this data
into just cases for Afghanistan:
@examples[#:eval ev #:label #f
  (~> tidy-who
      (where (country) (string=? country "Afghanistan"))
      show)
]
@racket[where] works similarly to each clause of @racket[create] or @racket[aggregate] -- we bind the
variable @tt{country} as an element, and then filter the rows of the data-frame for those where the
body returns true.

Then, if we try to plot this stratified by age, we run into an issue:
@examples[#:eval ev #:label #f
  (~> tidy-who
      (where (country) (string=? country "Afghanistan"))
      (graph #:data _
             #:mapping (aes #:x "year" #:y "cases" #:discrete-color "age")
             (lines)))
]

This is a giant mess! The issue is the @tt{sex} variable -- Graphite doesn't know what to do with this
information, since you end up with multiple data-points for year/case. So, we want to group by year and
age, sum the case data within them, and then ungroup: this effectively eliminates the sex variable.
@examples[#:eval ev #:label #f
  (~> tidy-who
      (where (country) (string=? country "Afghanistan"))
      (group-with "year" "age")
      (aggregate [cases (cases) (sum cases)])
      ungroup
      (graph #:data _
             #:mapping (aes #:x "year" #:y "cases" #:discrete-color "age")
             (lines)))
]

Finally, we have a plot, and we can determine that Afghanistan had a sudden spike in TB cases among ages
0-14 in 2010-2013.

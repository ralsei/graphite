#lang at-exp slideshow
(require data-frame
         graphite
         math/statistics
         (except-in pict/conditional show)
         (prefix-in pict: pict/conditional)
         pict/flash
         pict/shadow
         ppict/2
         ppict/slideshow2
         sawzall
         slideshow/code
         slideshow/staged-slide
         slideshow/text
         slideshow-text-style
         threading

         "gapminder-plot.rkt"
         "gapminder-graphite.rkt")

(define *global-font* "Source Sans Pro")
(define *mono-font* "Julia Mono")

(current-main-font *global-font*)
(current-code-font *mono-font*)
(get-current-code-font-size (thunk 20)) ;; ???

(sawzall-show-formatter
 (λ (val)
   (if (rational? val)
       (~r val #:precision 3)
       (~a val))))

;;;; helper functions
;; i am so tired
(define (frame p)
  (refocus (cc-superimpose
            p
            (rounded-rectangle (* 1.2 (pict-width p))
                               (* 1.2 (pict-height p))))
           p))

(define-syntax-rule (pslide/staged [name ...] arg ...)
  (staged [name ...] (pslide arg ...)))

(define (authors whos where)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([author-name #:size 30 #:color "blue"]
     [institution #:size 20])

    (vc-append (current-line-sep)
               (apply hc-append 50
                      (for/list ([who (in-list whos)])
                        (colorize (author-name who) "blue")))
               (blank (/ (current-font-size) 3))
               (scale/improve-new-text (institution where) 0.8))))

(define (take* lst n)
  (if (> n (length lst))
      lst
      (take lst n)))

;; do we need to specify what these are?
;; we can probably just say it verbally
(define (v/ vec c) (for/vector ([v (in-vector vec)]) (if v (/ v c) #f)))
(define (sum vec) (for/sum ([v (in-vector vec)] #:when v) v))

(define (df-show-slide df name csv-file [lst #f])
  (define defined-to (string->symbol (string-downcase name)))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([mono #:size 20 #:face *mono-font*])

    (pslide
     #:name (string-append "Dataframe: " name)
     #:go (coord 0.05 0.05 'lt)
     (code
      (define #,defined-to
        (df-read/csv #,(string-append "../data/" csv-file)
                     #:na "NA")))
     #:go (coord 0.95 0.95 'rb)
     (show-pict df lst))))

(define (show-pict df [lst #f] #:font-size [font-size 20] #:no-header? [no-header? #f])
  (define to-show
    (if (not lst)
        (take* (df-series-names df) 6)
        lst))
  (define described (string-split (with-output-to-string (thunk (show df (all-in to-show)))) "\n"))
  (define lines
    (if no-header?
        (drop described 1)
        described))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([mono #:size font-size #:face *mono-font*])
    (apply vl-append (current-line-sep)
           (for/list ([d (in-list lines)]
                      [_ (in-range (if no-header? 15 16))]) ; hack to hide help text
             (mono d)))))

;;;; data
;;;; likely that some of this is unused by the time the talk rolls around
(define gapminder (df-read/csv "../data/all_gapminder.csv" #:na "NA"))
(df-del-series! gapminder "")
(define oecd (df-read/csv "../data/oecd.csv" #:na "NA"))
(df-del-series! oecd "")
(define gss-sm (rename (df-read/csv "../data/gss_sm.csv" #:na "NA") "" "id"))
(define billboard (df-read/csv "../data/billboard.csv" #:na "NA"))
(define organdata (df-read/csv "../data/organdata.csv" #:na "NA"))
(define midwest (df-read/csv "../data/midwest.csv" #:na "NA"))
(define anscombe
  (row-df [x1 x2 x3 x4 y1    y2   y3    y4]
          10  10 10 8  8.04  9.14 7.46  6.58
          8   8  8  8  6.95  8.14 6.77  5.76
          13  13 13 8  7.58  8.74 12.74 7.71
          9   9  9  8  8.81  8.77 7.11  8.84
          11  11 11 8  8.33  9.26 7.81  8.47
          14  14 14 8  9.96  8.10 8.84  7.04
          6   6  6  8  7.24  6.13 6.08  5.25
          4   4  4  19 4.26  3.10 5.39  12.50
          12  12 12 8  10.84 9.13 8.15  5.56
          7   7  7  8  4.82  7.26 6.42  7.91
          5   5  5  8  5.68  4.74 5.73  6.89))

;;;; actual slides
(define (title-slide)
  (define midwest-plot
    (graph #:data midwest
           #:mapping (aes #:x "area")
           #:width (get-client-w) #:height (get-client-h)
           (density #:mapping (aes #:discrete-color "state"))))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([heading #:size 50 #:bold? #t]
     [subheading #:size 30 #:color "firebrick"])

    ;; make the thing the fucking Thing the
    (pslide
     (cc-superimpose
      (filled-rectangle (get-client-w) (get-client-h)
                        #:draw-border? #f
                        #:color "white")
      (cellophane midwest-plot 0.1))
     #:go (coord 0.5 0.4 'cc)
     @heading{Graphite: A Library for Data Visualization}
     #:go (coord 0.5 0.6 'cc)
     (authors '("Hazel Levine" "Sam Tobin-Hochstadt") "Indiana University"))))

(define (intro-slides)
  (define ggplot2-logo (scale-to-fit (bitmap "ggplot2-logo.png") 200 200))
  (define racket-logo (scale-to-fit (bitmap "racket-logo.png") 200 200))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [t #:size 25]
     [ti #:size 25
         #:transform (λ (p) (t #:h-append hc-append #:left-pad 30 "• " p))])

    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{What is Graphite?}
     #:go (coord 0.05 0.5 'lc)
     (vl-append
      (current-line-sep)
      @ti{a Racket library for drawing statistical plots}
      @ti{based on the grammar of graphics}
      @ti{inspired by R's ggplot2 and Julia's Gadfly}
      @ti{output: Racket picts}
      @ti{convertible to PDF, PNG, SVG})
     #:go (coord 0.75 0.5 'cc)
     (pin-arrow-line 30 (vc-append 100 ggplot2-logo racket-logo)
                     ggplot2-logo cb-find
                     racket-logo  ct-find
                     #:line-width 3
                     #:color "cadet blue"))))

(define (gdpPercap-lifeExp-slides)
  (df-show-slide gapminder "Gapminder" "all_gapminder.csv")

  (define (make-gapminder-example x-lab y-lab trans points fit)
    (code
     (graph #:data gapminder
            #:mapping (aes #:x "gdpPercap"
                           #:y "lifeExp")
            #,x-lab
            #,y-lab
            #,trans
            #,fit
            #,points)))

  (define initial-example
    (graph #:data gapminder
           #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
           (points)))
  (define log-transformed-example
    (graph #:data gapminder
           #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
           #:x-label "GDP per capita (USD)"
           #:y-label "Life expectancy (years)"
           #:x-transform logarithmic-transform
           (points #:alpha 0.4)))
  (define colorized-example
    (graph #:data gapminder
           #:mapping (aes #:x "gdpPercap" #:y "lifeExp")
           #:x-label "GDP per capita (USD)"
           #:y-label "Life expectancy (years)"
           #:x-transform logarithmic-transform
           (points #:alpha 0.4 #:mapping (aes #:discrete-color "continent"))
           (fit #:width 3 #:method 'loess)))

  (slide/staged
   [initial log-transformed colorized]
   (hc-append
    50
    (pict-case stage-name #:combine lt-superimpose
               [(initial log-transformed)
                (make-gapminder-example
                 (pict:show (code #:x-label "GDP per capita (USD)")
                            (at/after log-transformed))
                 (pict:show (code #:y-label "Life expectancy (years)")
                            (at/after log-transformed))
                 (pict:show (frame (code #:x-transform logarithmic-transform))
                            (at/after log-transformed))
                 (case stage-name
                   [(initial) (code (points))]
                   [(log-transformed) (code (points #,(frame (code #:alpha 0.4))))])
                 (ghost (code (fit #:width 3 #:method 'loess))))]
               [(colorized)
                (make-gapminder-example
                 (code #:x-label "GDP per capita (USD)")
                 (code #:y-label "Life expectancy (years)")
                 (code #:x-transform logarithmic-transform)
                 (code (points #:alpha 0.4
                               #,(frame
                                  (code
                                   #:mapping
                                   (aes #:discrete-color
                                        "continent")))))
                 (frame (code (fit #:width 3 #:method 'loess))))])
    (pict-case stage-name #:combine cc-superimpose
               [(initial) initial-example]
               [(log-transformed) log-transformed-example]
               [(colorized) colorized-example]))))

(define (prior-work-slides)
  (define ggplot2-logo (scale-to-fit (bitmap "ggplot2-logo.png") 150 150))
  (define racket-logo (scale-to-fit (bitmap "racket-logo.png") 150 150))

  (define gapminder-plot-source (file->string "gapminder-plot.rkt"))
  (define gapminder-graphite-source (file->string "gapminder-graphite.rkt"))
  (define gapminder-ggplot2-source (file->lines "gapminder-r.r"))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [titlett #:size 50 #:bold? #t #:face *mono-font*]
     [t #:size 25]
     [tt #:size 15 #:face *mono-font*]
     [tit #:size 25 #:italic? #t]
     [ti #:size 25
         #:transform (λ (p) (t #:h-append hc-append #:left-pad 30 "• " p))]
     [cred #:size 10])
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{@titlett{ggplot2} and R}
     (vl-append
      (current-line-sep)
      @ti{a popular R library for data visualization}
      @ti{designed around the grammar of graphics}
      @ti{part of the @tit{tidyverse}, a software package based around @tit{tidy data}})
     #:go (coord 0.03 0.55 'lc)
     (apply vl-append
            (for/list ([v (in-list gapminder-ggplot2-source)])
              (tt v)))
     #:go (coord 0.97 0.55 'rc)
     (parameterize ([get-current-code-font-size (thunk 15)])
       (codeblock-pict gapminder-graphite-source))
     #:go (coord 0.25 0.97 'cb)
     ggplot2-logo
     #:go (coord 0.75 0.97 'cb)
     racket-logo)

    (pslide/staged
     [none one two three]
     #:go (coord 0.05 0.05 'lt)
     @title{What is tidy data?}
     #:go (coord 0.5 0.2 'ct)
     (vl-append
      (current-line-sep)
      @ti{@tit{Idea:} data-frames are just a record of vectors...}
      @ti{...and we work with data by-variable...}
      @ti{...so you should be able to get a variable by selecting from the record.})
     #:go (coord 0.05 0.66 'lc)
     (vl-append
      (current-line-sep)
      @ti{@tit{To accomplish this:}}
      (pict:show @ti{Every column is exactly one variable.}
                 (at/after one))
      (pict:show @ti{Every row is exactly one observation.}
                 (at/after two))
      (pict:show @ti{Every cell is exactly one value.}
                 (at/after three)))
     #:go (coord 0.95 0.66 'rc)
     (vc-append
      (scale-to-fit
       (pict-case stage-name #:combine cc-superimpose
                  [(none) (blank)]
                  [(one) (bitmap "tidy-0-0.png")]
                  [(two) (bitmap "tidy-1-0.png")]
                  [(three) (bitmap "tidy-2-0.png")])
       310 300)
      (pict:show @cred{Credit: R for Data Science, chapter 12}
                 (after none))))

    (pslide/staged
     [no-boxes one-box two-box]
     #:go (coord 0.05 0.05 'lt)
     @title{Racket plot vs Graphite}
     (cc-superimpose
      (parameterize ([get-current-code-font-size (thunk 12)])
        (codeblock-pict gapminder-plot-source))
      (pict:show
       (rotate
        (shadow-frame @t{41 LOC})
        (* pi 1/8))
       (at/after one-box)))
     #:go (coord 0.99 0.56 'rc)
     (cc-superimpose
      (parameterize ([get-current-code-font-size (thunk 15)])
        (codeblock-pict gapminder-graphite-source))
      (pict:show
       (rotate
        (shadow-frame
         @t{
           18 LOC!
           @tit{(and way less cognitive load)}
         })
        (* pi 1/8))
       (at/after two-box))))))


(define (year-gdpPercap-slides)
  (define (make-fig44-example mapping y-trans renderers)
    (code (graph #:data gapminder
                 #:mapping #,mapping
                 #,y-trans
                 #,renderers)))

  (define initial-example
    (graph #:data gapminder
           #:mapping (aes #:x "year"
                          #:y "gdpPercap")
           #:legend-anchor 'no-legend
           (lines #:mapping (aes #:discrete-color "country"))))
  (define by-continent-example
    (graph #:data gapminder
           #:mapping (aes #:x "year"
                          #:y "gdpPercap"
                          #:facet "continent")
           #:facet-wrap 5
           #:width (get-client-w)
           #:height 450
           #:legend-anchor 'no-legend
           (lines #:mapping (aes #:discrete-color "country"))))
  (define y-transformed-example
    (graph #:data gapminder
           #:mapping (aes #:x "year"
                          #:y "gdpPercap"
                          #:facet "continent")
           #:facet-wrap 5
           #:width (get-client-w)
           #:height 450
           #:y-transform logarithmic-transform
           #:legend-anchor 'no-legend
           (lines #:mapping (aes #:discrete-color "country")
                  #:color "gray")))
  (define fitted-example
    (graph #:data gapminder
           #:mapping (aes #:x "year"
                          #:y "gdpPercap"
                          #:facet "continent")
           #:facet-wrap 5
           #:width (get-client-w)
           #:height 450
           #:y-transform logarithmic-transform
           #:legend-anchor 'no-legend
           (lines #:mapping (aes #:discrete-color "country")
                  #:color "gray")
           (fit #:width 3 #:method 'loess)))

  (slide/staged
   [initial by-continent y-transformed fitted]
   (vc-append
    50
    (pict-case stage-name #:combine lt-superimpose
               [(initial by-continent)
                (make-fig44-example
                 (pict-if #:combine lt-superimpose
                          (at initial)
                          (code (aes #:x "year"
                                     #:y "gdpPercap"))
                          (code (aes #:x "year"
                                     #:y "gdpPercap"
                                     #,(frame (code #:facet "continent")))))
                 (ghost (code #:y-transform logarithmic-transform))
                 (code (lines #:mapping (aes #:discrete-color
                                             "country"))))]
               [(y-transformed)
                (make-fig44-example
                 (code (aes #:x "year"
                            #:y "gdpPercap"
                            #:facet "continent"))
                 (frame (code #:y-transform logarithmic-transform))
                 (code (lines #:mapping (aes #:discrete-color
                                             "country")
                              #,(frame (code #:color "gray")))))]
               [(fitted)
                (make-fig44-example
                 (code (aes #:x "year"
                            #:y "gdpPercap"
                            #:facet "continent"))
                 (code #:y-transform logarithmic-transform)
                 (code (lines #:mapping (aes #:discrete-color
                                             "country")
                              #:color "gray")
                       #,(frame (code (fit #:width 3 #:method 'loess)))))])
    (pict-case stage-name #:combine cc-superimpose
               [(initial) initial-example]
               [(by-continent) by-continent-example]
               [(y-transformed) y-transformed-example]
               [(fitted) fitted-example]))))

(define (sawzall-intro-slides)
  (define dplyr-logo (scale-to-fit (bitmap "dplyr-logo.png") 200 200))
  (define tidyr-logo (scale-to-fit (bitmap "tidyr-logo.png") 200 200))
  (define racket-logo (scale-to-fit (bitmap "racket-logo.png") 200 200))

  (define bang (cc-superimpose (colorize (filled-flash 100 100) "red")
                               (colorize (filled-flash 70 70) "orange")))
  (define dplyr+tidyr+bang (vc-append 50 (hc-append 30 dplyr-logo tidyr-logo) bang))
  (define dplyr+tidyr->bang
    (pin-arrow-line 30 (pin-arrow-line 30 dplyr+tidyr+bang
                                       dplyr-logo cc-find
                                       bang ct-find
                                       #:under? #t
                                       #:line-width 3
                                       #:color "cadet blue")
                    tidyr-logo cc-find
                    bang ct-find
                    #:under? #t
                    #:line-width 3
                    #:color "cadet blue"))
  (define dplyr+tidyr->bang->racket
    (pin-arrow-line 30 (vc-append 50 dplyr+tidyr->bang racket-logo)
                    dplyr+tidyr->bang cb-find
                    racket-logo ct-find
                    #:line-width 3
                    #:color "cadet blue"))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [t #:size 25]
     [ti #:size 25
         #:transform (λ (p) (t #:h-append hc-append #:left-pad 30 "• " p))]
     [tt #:size 25 #:face *mono-font*])

    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{What is Sawzall?}
     #:go (coord 0.05 0.5 'lc)
     (vl-append
      (current-line-sep)
      @ti{a Racket library for data manipulation}
      @ti{like map/filter/fold, but for tabular data}
      @ti{designed to be compositional, with the @tt{~>} operator}
      @ti{inspired by R's dplyr+tidyr and Julia's DataFrames.jl})
     #:go (coord 0.75 0.5 'cc)
     dplyr+tidyr->bang->racket)))

(define (gss-pipeline-slides)
  (df-show-slide gss-sm "gss-sm" "gss_sm.csv"
                 '("bigregion" "religion" "kids" "ageq" "sex" "marital"))

  (define the-arrow (cc-superimpose
                     (arrowhead 56 0)
                     (colorize (arrowhead 50 0) "pale green")))
  (with-text-style
    #:defaults [#:face *global-font*]
    ([df-title #:size 25 #:bold? #t])

    (pslide/staged
     [step-1 step-2 step-3]
     #:go (tile 3 1)
     (vc-append
      @df-title{
        Individual-level data
        on region and religion
      }
      (show-pict gss-sm '("id" "bigregion" "religion")
                 #:font-size 15 #:no-header? #t))
     (pict:show
      (vc-append
       @df-title{
         Summary count by region of
         religious preferences
       }
       (show-pict (~> gss-sm
                      (group-with "bigregion" "religion")
                      (aggregate [count (bigregion) (vector-length bigregion)])
                      ungroup)
                  '("bigregion" "religion" "count")
                  #:font-size 15 #:no-header? #t))
      (at/after step-2))
     (pict:show
      (vc-append
       @df-title{
          Percent religious preference
          by census region
       }
       (show-pict (~> gss-sm
                      (group-with "bigregion" "religion")
                      (aggregate [count (bigregion) (vector-length bigregion)])
                      (create [freq ([count : vector]) (v/ count (sum count))]
                              [pct (freq) (round (* freq 100))])
                      ungroup)
                  '("bigregion" "religion" "pct")
                  #:font-size 15 #:no-header? #t))
      (at/after step-3))
     #:go (coord 0.325 0.5 'cc)
     (pict:show the-arrow (at/after step-2))
     #:go (coord 0.675 0.5 'cc)
     (pict:show the-arrow (at/after step-3)))))

(define (gss-example-slides)
  (define (make-gss-example level)
    (code
     (~> gss-sm
         (group-with "bigregion" "religion")
         #,(pict:show (code (aggregate [count (bigregion) (vector-length bigregion)]))
                      (>= level 2))
         #,(pict:show (code (create [frequency ([count : vector]) (v/ count (sum count))]
                                    #,(pict:show (code [percent (frequency) (round (* frequency 100))])
                                                 (>= level 4))))
                      (>= level 3))
         ungroup
         show)))

  (slide/staged
   [initial agg create-1 create-2]
   (vc-append
    50
    (make-gss-example stage)
    (pict-case stage-name #:combine lc-superimpose
               [(initial) (show-pict (~> gss-sm
                                         (group-with "bigregion" "religion")
                                         ungroup)
                                     '("id" "bigregion" "religion"))]
               [(agg) (show-pict (~> gss-sm
                                     (group-with "bigregion" "religion")
                                     (aggregate [count (bigregion) (vector-length bigregion)])
                                     ungroup)
                                 '("bigregion" "religion" "count"))]
               [(create-1) (show-pict (~> gss-sm
                                          (group-with "bigregion" "religion")
                                          (aggregate [count (bigregion) (vector-length bigregion)])
                                          (create [frequency ([count : vector]) (v/ count (sum count))])
                                          ungroup)
                                      '("bigregion" "religion" "count" "frequency"))]
               [(create-2) (show-pict (~> gss-sm
                                          (group-with "bigregion" "religion")
                                          (aggregate [count (bigregion) (vector-length bigregion)])
                                          (create [frequency ([count : vector]) (v/ count (sum count))]
                                                  [percent (frequency) (round (* frequency 100))])
                                          ungroup)
                                      '("bigregion" "religion" "count" "frequency" "percent"))])))

  (define religion-by-region
    (~> gss-sm
        (group-with "bigregion" "religion")
        (aggregate [count (bigregion) (vector-length bigregion)])
        (create [frequency ([count : vector]) (v/ count (sum count))]
                [percent (frequency) (round (* frequency 100))])
        ungroup))

  (define the-graph
    (graph #:data religion-by-region
           #:mapping (aes #:x "religion"
                          #:y "percent"
                          #:facet "bigregion")
           #:width 700 #:height 700
           (col)))

  (slide
   (code (graph #:data religion-by-region
                #:mapping (aes #:x "religion"
                               #:y "percent"
                               #:facet "bigregion")
                #:width 700 #:height 700
                (col))))
  (slide the-graph))

(define (billboard-example-slides)
  (df-show-slide billboard "billboard" "billboard.csv"
                 '("track" "artist" "date.entered" "wk1" "wk2" "wk3"))

  (define (make-billboard-example pivot create)
    (code (~> billboard
              #,pivot
              #,create
              show)))

  (pslide/staged
   [initial pivoted created]
   (vc-append
    50
    (make-billboard-example
     (pict:show (code (pivot-longer (starting-with "wk")
                                    #:names-to "week"
                                    #:values-to "ranking"))
                (at/after pivoted))
     (pict:show (code (create [week (week) (string->number (substring week 2))]))
                (at/after created)))
    (pict-case stage-name #:combine lc-superimpose
               [(initial) (show-pict billboard '("track" "artist" "date.entered" "wk1" "wk2" "wk3"))]
               [(pivoted) (show-pict (~> billboard
                                         (pivot-longer (starting-with "wk")
                                                       #:names-to "week"
                                                       #:values-to "ranking")
                                         (reorder "week")) ; for display purposes, avoiding #fs
                                     '("track" "artist" "date.entered" "week" "ranking"))]
               [(created) (show-pict (~> billboard
                                         (pivot-longer (starting-with "wk")
                                                       #:names-to "week"
                                                       #:values-to "ranking")
                                         (reorder "week")
                                         (create [week (week) (string->number (substring week 2))]))
                                     '("track" "artist" "date.entered" "week" "ranking"))])))

  (define tidy-billboard
    (~> billboard
        (pivot-longer (starting-with "wk")
                      #:names-to "week"
                      #:values-to "ranking")
        (create [week (week) (string->number (substring week 2))])))

  (define (make-billboard-graph-example where drop-na sorting ending)
    (code (~> tidy-billboard
              #,where
              #,drop-na
              #,sorting
              #,ending)))

  (pslide/staged
   [initial filtered dropped graphed]
   (vc-append
    50
    (pict-case stage-name #:combine lt-superimpose
               [(initial filtered dropped)
                (make-billboard-graph-example
                 (pict:show (code (where (artist) (string=? artist "Blink-182")))
                            (at/after filtered))
                 (pict:show (code (drop-na "ranking"))
                            (at/after dropped))
                 (pict:show (code (reorder "week"))
                            (at/after dropped))
                 (code show))]
               [(graphed)
                (make-billboard-graph-example
                 (code (where (artist) (string=? artist "Blink-182")))
                 (code (drop-na "ranking"))
                 (code (reorder "week"))
                 (code (graph #:data _
                              #:mapping (aes #:x "week" #:y "ranking")
                              (lines))))])
    (pict-case stage-name #:combine cc-superimpose
               [(initial) (show-pict tidy-billboard)]
               [(filtered) (show-pict (~> tidy-billboard
                                          (where (artist) (string=? artist "Blink-182"))))]
               [(dropped) (show-pict (~> tidy-billboard
                                         (where (artist) (string=? artist "Blink-182"))
                                         (drop-na "ranking")
                                         (reorder "week")))]
               [(graphed) (~> tidy-billboard
                              (where (artist) (string=? artist "Blink-182"))
                              (drop-na "ranking")
                              (reorder "week")
                              (graph #:data _
                                     #:mapping (aes #:x "week" #:y "ranking")
                                     (lines)))]))))

(define (bunch-of-plots-slide)
  (define (scale-to-4-panel p)
    (scale-to-fit p (/ (get-client-w) 2) (/ (get-client-h) 2)))

  (define oecd-plot
    (scale-to-4-panel
     (graph #:data oecd
            #:mapping (aes #:x "year" #:y "diff")
            #:title "Difference between US and OECD average life expectancies"
            #:x-label "Year" #:y-label "Difference (years)"
            #:y-min -2 #:y-max 2
            #:width 600 #:height 400
            #:legend-anchor 'no-legend
            (col #:mapping (aes #:discrete-color "hi_lo")))))

  (define anscombe-facetable
    (~> anscombe
        (create [nrow ([x1 : vector]) (build-vector (vector-length x1) (λ (x) x))])
        (pivot-longer (not "nrow") #:names-to "name" #:values-to "val")
        (separate "name" #:into '("x-or-y" "quadrant") #:separator 1)
        (pivot-wider #:names-from "x-or-y" #:values-from "val")
        (slice (not "nrow"))))
  (define anscombe-plot
    (scale-to-4-panel
     (graph #:data anscombe-facetable
            #:mapping (aes #:x "x" #:y "y" #:facet "quadrant")
            #:width 600 #:height 470    ; weh
            #:title "Anscombe's Quartet"
            (points)
            (fit #:width 3))))

  (define another-gss-plot
    (scale-to-4-panel
     (graph #:data gss-sm
            #:title "Religious preferences among regions, GSS 2016"
            #:mapping (aes #:x "bigregion" #:group "religion")
            #:width 600 #:height 400
            (stacked-bar #:mode 'prop))))

  (define sorted-countries
    (~> organdata
        (group-with "country")
        (aggregate [med (donors) (median < (vector-filter identity donors))])
        (reorder "med")
        (df-select "country")))
  (define organdata-sorted
    (reorder organdata (cons "country" (by-vector sorted-countries))))
  (define organdata-plot
    (scale-to-4-panel
     (graph #:data organdata-sorted
            #:title "Organ donation count by country, over time"
            #:mapping (aes #:x "donors" #:y "country")
            #:width 600 #:height 400
            (boxplot #:invert? #t))))

  (pslide
   #:go (tile 2 2)
   oecd-plot
   anscombe-plot
   another-gss-plot
   organdata-plot))

(define (end-slide)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [gh-link #:size 30 #:color "blue"])
    (pslide
     #:go (coord 0.5 0.1 'cc)
     @title{Thank you!}
     #:go (coord 0.5 0.5 'cc)
     (bitmap "nickelback.png")
     #:go (coord 0.5 0.9 'cc)
     (hyperlinkize @gh-link{https://github.com/ralsei/graphite}))))

;;;; main
(module+ main
  (title-slide)
  #;(intro-slides)
  (gdpPercap-lifeExp-slides)
  (prior-work-slides)
  (year-gdpPercap-slides)
  (sawzall-intro-slides)
  (gss-pipeline-slides)
  (gss-example-slides)
  (billboard-example-slides)
  (bunch-of-plots-slide)
  (end-slide))

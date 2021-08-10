#lang at-exp slideshow
(require data-frame
         iu-pict
         graphite
         (except-in pict/conditional show)
         (prefix-in pict: pict/conditional)
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
        (df-read/csv #,(string-append "data/" csv-file)
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
(define gapminder (df-read/csv "data/all_gapminder.csv" #:na "NA"))
(df-del-series! gapminder "")
(define oecd (df-read/csv "data/oecd.csv" #:na "NA"))
(df-del-series! oecd "")
(define gss-sm (rename (df-read/csv "data/gss_sm.csv" #:na "NA") "" "id"))
#;(df-del-series! gss-sm "")

;;;; actual slides
(define (title-slide)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([heading #:size 50 #:bold? #t]
     [subheading #:size 30 #:color "firebrick"])

    (pslide
     #:name "Title"
     #:go (coord 0.5 0.4 'cc)
     @heading{Graphite: A Library for Data Visualization}
     (blank)
     @subheading{Implementing the grammar of graphics in Racket}
     #:go (coord 0.5 0.6 'cc)
     (authors '("Hazel Levine" "Sam Tobin-Hochstadt") "Indiana University")
     #:go (coord 0.95 0.95 'rb)
     (iu-logo 60))))

(define (intro-slides)
  (define ggplot2-logo (scale-to-fit (bitmap "ggplot2-logo.png") 200 200))
  (define racket-logo (scale-to-fit (bitmap "racket-logo.png") 200 200))

  (define gapminder-source (file->string "gapminder-plot.rkt"))

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

  (define gapminder-plot-source (file->string "gapminder-plot.rkt"))
  (define gapminder-graphite-source (file->string "gapminder-graphite.rkt"))

  (define (make-gapminder-example x-lab y-lab trans points fit [facet (blank)])
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
  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [t #:size 25]
     [ti #:size 25
         #:transform (λ (p) (t #:h-append hc-append #:left-pad 30 "• " p))])
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{@titleit{Why} is Graphite?}
     (parameterize ([get-current-code-font-size (thunk 12)])
       (codeblock-pict gapminder-plot-source))
     #:go (coord 0.95 0.5 'rc)
     gapminder-plot))

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
               [(colorized) colorized-example])))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [t #:size 25]
     [it #:size 25 #:italic? #t])
    (pslide/staged
     [no-boxes one-box two-box]
     #:go (coord 0.05 0.05 'lt)
     @title{@titleit{That's} why.}
     (cc-superimpose
      (parameterize ([get-current-code-font-size (thunk 12)])
        (codeblock-pict gapminder-plot-source))
      (pict:show
       (rotate
        (shadow-frame @t{41 LOC})
        (* pi 1/8))
       (at/after one-box)))
     #:go (coord 0.95 0.56 'rc)
     (cc-superimpose
      (parameterize ([get-current-code-font-size (thunk 15)])
        (codeblock-pict gapminder-graphite-source))
      (pict:show
       (rotate
        (shadow-frame
         @t{
           18 LOC!
           @it{(and way less cognitive load)}
         })
        (* pi 1/8))
       (at/after two-box)))))

  (slide
   (code
    (graph #:data gapminder
           #:mapping (aes #:x "gdpPercap"
                          #:y "lifeExp"
                          #,(frame (code #:facet "continent")))
           #:x-label "GDP per capita (USD)"
           #:y-label "Life expectancy (years)"
           #:x-transform logarithmic-transform
           (fit #:width 3 #:method 'loess)
           (points #:alpha 0.4))))
  (slide
   (graph #:data gapminder
          #:mapping (aes #:x "gdpPercap"
                         #:y "lifeExp"
                         #:facet "continent")
          #:x-label "GDP per capita (USD)"
          #:y-label "Life expectancy (years)"
          #:x-transform logarithmic-transform
          ;; the dreaded layout engine bug...
          #:width (- (get-client-w) 100) #:height (- (get-client-h) 100)
          (fit #:width 3 #:method 'loess)
          (points #:alpha 0.4))))

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
           (lines)))
  (define by-continent-example
    (graph #:data gapminder
           #:mapping (aes #:x "year"
                          #:y "gdpPercap"
                          #:facet "continent")
           #:facet-wrap 1
           #:width (inexact->exact (round (/ (get-client-w) 2.1)))
           #:height (+ (get-client-h) 150)
           (lines)))

  (slide/staged
   [initial by-continent y-transformed fitted]
   (hc-append
    (make-fig44-example
     (code (aes #:x "year"
                #:y "gdpPercap"
                #:facet "continent"))
     (ghost (code #:y-transform logarithmic-transform))
     (code (lines)))
    by-continent-example
    #;initial-example)))


;; could be dead code -- determine if we want to include this later
;; it looks nice but doesn't have the "incremental" approach we want
(define (oecd-example-slides)
  (df-show-slide oecd "oecd" "oecd.csv")

  (pslide
   #:go (coord 0.5 0.05 'ct)
   (code
    (graph #:data oecd
           #:mapping (aes #:x "year" #:y "diff")
           #:title "Difference between US and OECD average life expectancies"
           #:x-label "Year" #:y-label "Difference (years)"
           #:y-min -2 #:y-max 2
           #:width 600 #:height 400
           #:legend-anchor 'no-legend
           (col #:mapping (aes #:discrete-color "hi_lo"))))
   #:go (coord 0.5 0.95 'cb)
   (graph #:data oecd
          #:mapping (aes #:x "year" #:y "diff")
          #:title "Difference between US and OECD average life expectancies"
          #:x-label "Year" #:y-label "Difference (years)"
          #:y-min -2 #:y-max 2
          #:width 600 #:height 400
          #:legend-anchor 'no-legend
          (col #:mapping (aes #:discrete-color "hi_lo")))))

(define (gss-example-slides)
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
  #;(title-slide)
  #;(intro-slides)
  #;(gdpPercap-lifeExp-slides)
  (year-gdpPercap-slides)
  #;(oecd-example-slides)
  #;(gss-example-slides)
  (end-slide))

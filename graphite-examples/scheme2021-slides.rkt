#lang at-exp slideshow
(require data-frame
         iu-pict
         graphite
         ppict/2
         ppict/slideshow2
         sawzall
         slideshow/code
         slideshow/text
         slideshow-text-style)

(define *global-font* "Source Sans Pro")
(define *mono-font* "Julia Mono")

(current-main-font *global-font*)
(current-code-font *mono-font*)
(get-current-code-font-size (thunk 25)) ;; ???

;;;; helper functions
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

(define (df-describe-slide df name csv-file)
  (define defined-to (string->symbol (string-downcase name)))
  (define described (string-split (with-output-to-string (thunk (df-describe df))) "\n"))
  (with-text-style
    #:defaults [#:face *global-font*]
    ([heading #:size 50 #:bold? #t]
     [mono #:size 20 #:face *mono-font*])

    (pslide
     #:name (string-append "Dataframe: " name)
     #:go (coord 0.05 0.05 'lt)
     (heading (string-append name " dataset"))

     #:go (tile 1 2)
     (code
      (define #,defined-to
        (df-read/csv #,(string-append "data/" csv-file)
                     #:na "NA"))
      (df-describe #,defined-to))

     (apply vl-append (current-line-sep)
            (for/list ([d (in-list described)])
              (mono d))))))

(define-syntax-rule (graph-example-slide body extras)
  (graph-example-slide/int (code body) body extras))

(define (graph-example-slide/int stx-pict graph-pict extras)
  (define stx-h (pict-height stx-pict))
  (define graph-h (pict-height graph-pict))

  (define stx-scaled
    (if (< stx-h graph-h)
        (scale-to-fit stx-pict graph-pict)
        stx-pict))
  (define graph-scaled
    (if (< graph-h stx-h)
        (scale-to-fit graph-pict stx-pict)
        graph-pict))

  (pslide
   #:name "Graph example"
   #:go (tile 2 1)
   (scale stx-scaled 1.2)
   graph-scaled))

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

;;;; examples
(define gapminder (df-read/csv "data/all_gapminder.csv" #:na "NA"))
(define oecd (df-read/csv "data/organdata.csv" #:na "NA"))

(define (gapminder-example-slides)
  (df-describe-slide gapminder "Gapminder" "all_gapminder.csv")
  (graph-example-slide
   (graph #:data gapminder
          #:mapping (aes #:x "gdpPercap"
                         #:y "lifeExp")
          (points))
   @para{when you drink water})
  (graph-example-slide
   (graph #:data gapminder
          #:x-label "GDP per capita (USD)"
          #:y-label "Life expectancy (years)"
          #:mapping (aes #:x "gdpPercap"
                         #:y "lifeExp")
          #:x-transform logarithmic-transform
          (points #:alpha 0.4)
          (fit #:width 3))
   (blank))
  (graph-example-slide
   (graph #:data gapminder
          #:x-label "GDP per capita (USD)"
          #:y-label "Life expectancy (years)"
          #:mapping (aes #:x "gdpPercap"
                         #:y "lifeExp")
          #:x-transform logarithmic-transform
          (points #:alpha 0.4
                  #:mapping
                  (aes #:discrete-color
                       "continent"))
          (fit #:width 3))
   (blank)))

;;;; main
(module+ main
  (title-slide)
  (gapminder-example-slides))

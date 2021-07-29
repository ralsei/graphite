#lang at-exp slideshow/widescreen
(require (for-syntax racket/base)
         data-frame
         iu-pict
         graphite
         racket/stxparam
         sawzall
         slideshow/code
         slideshow/text
         syntax/parse/define)

;;;; helper functions

(define (authors whos where)
  (vc-append (current-line-sep)
             (apply hc-append 50
                    (for/list ([who (in-list whos)])
                      (colorize (bt who) "blue")))
             (blank (/ (current-font-size) 3))
             (scale/improve-new-text (t where) 0.8)))

(define (df-describe-slide df name csv-file)
  (define defined-to (string->symbol (string-downcase name)))
  (define described (string-split (with-output-to-string (thunk (df-describe df))) "\n"))
  (slide
   #:name (string-append "Dataframe: " name)
   @titlet[name]
   (blank)
   (code
    (define #,defined-to
      (df-read/csv #,(string-append "data/" csv-file)
                   #:na "NA"))
    (df-describe #,defined-to))
   (with-size 20
     (apply vl-append (current-line-sep)
            (for/list ([d (in-list described)])
              (tt d))))))

(define-syntax-rule (graph-example-slide df body)
  (graph-example-slide/int (code body) body))

(define (graph-example-slide/int stx-pict graph-pict)
  (parameterize ([current-font-size 25])
    (slide
     (vc-append 30 stx-pict graph-pict))))

;;;; actual slides
(define (title-slide)
  (slide
   #:name "Title"
   @titlet{Graphite: A Library for Data Visualization}
   (parameterize ([current-titlet (λ (s)
                                    (colorize (text s (current-main-font) 30)
                                              (current-title-color)))])
     @titlet{Implementing the grammar of graphics in Racket})
   (blank)
   (iu-logo 60)
   (blank)
   (authors '("Hazel Levine" "Sam Tobin-Hochstadt") "Indiana University")))

;;;; examples
(define gapminder (df-read/csv "data/all_gapminder.csv" #:na "NA"))
(define oecd (df-read/csv "data/organdata.csv" #:na "NA"))

;;;; main
(module+ main
  (title-slide)
  (df-describe-slide gapminder "Gapminder" "all_gapminder.csv")
  (graph-example-slide
   gapminder
   (graph #:data gapminder
          #:mapping (aes #:x "gdpPercap"
                         #:y "lifeExp")
          (points)))
  (graph-example-slide
   gapminder
   (graph #:data gapminder
          #:title "GDP per capita vs life expectancy"
          #:x-label "GDP per capita (USD)"
          #:y-label "Life expectancy (years)"
          #:mapping (aes #:x "gdpPercap"
                         #:y "lifeExp")
          #:x-transform logarithmic-transform
          (points #:alpha 0.4)
          (fit #:width 3)))
  (graph-example-slide
   gapminder
   (graph #:data gapminder
          #:title "GDP per capita vs life expectancy"
          #:x-label "GDP per capita (USD)"
          #:y-label (aes #:x "gdpPercap"
                         #:y "lifeExp")
          #:x-transform logarithmic-transform
          (points #:alpha 0.4
                  #:mapping (aes #:discrete-color "continent"))
          (fit #:width 3))))

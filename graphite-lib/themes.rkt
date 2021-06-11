#lang racket/base
(require colormaps
         plot/utils
         racket/contract/base
         racket/struct)

(provide
 (contract-out [struct graphite-theme ((foreground plot-color/c)
                                       (foreground-alpha (real-in 0 1))
                                       (background plot-color/c)
                                       (background-alpha (real-in 0 1))
                                       (font-size (>=/c 0))
                                       (font-face (or/c string? #f))
                                       (font-family font-family/c)
                                       (pen-color-map (or/c symbol? #f))
                                       (brush-color-map (or/c symbol? #f)))])
 make-graphite-theme
 theme-override
 theme-default
 theme-continuous
 theme->alist)

(struct graphite-theme (foreground foreground-alpha
                        background background-alpha
                        font-size font-face font-family
                        pen-color-map brush-color-map) #:transparent)

(define (make-graphite-theme #:fg [fg (plot-foreground)]
                             #:fg-alpha [fg-alpha (plot-foreground-alpha)]
                             #:bg [bg (plot-background)]
                             #:bg-alpha [bg-alpha (plot-background-alpha)]
                             #:font-size [font-size (plot-font-size)]
                             #:font-face [font-face (plot-font-face)]
                             #:font-family [font-family (plot-font-family)]
                             #:color-map [color-map (plot-pen-color-map)]
                             #:brush-color-map [brush-color-map color-map])
  (graphite-theme fg fg-alpha bg bg-alpha
                  font-size font-face font-family
                  color-map brush-color-map))

(define (theme-override theme
                        #:fg [fg (graphite-theme-foreground theme)]
                        #:fg-alpha [fg-alpha (graphite-theme-foreground-alpha theme)]
                        #:bg [bg (graphite-theme-background theme)]
                        #:bg-alpha [bg-alpha (graphite-theme-background-alpha theme)]
                        #:font-size [font-size (graphite-theme-font-size theme)]
                        #:font-face [font-face (graphite-theme-font-face theme)]
                        #:font-family [font-family (graphite-theme-font-family theme)]
                        #:color-map [color-map (graphite-theme-pen-color-map theme)]
                        #:brush-color-map [brush-color-map (graphite-theme-brush-color-map theme)])
  (graphite-theme fg fg-alpha bg bg-alpha
                  font-size font-face font-family
                  color-map brush-color-map))

(define theme-default
  (make-graphite-theme #:fg "black" #:fg-alpha 1
                       #:bg "white" #:bg-alpha 1
                       #:font-size 11 #:font-family 'swiss
                       #:color-map 'set1 #:brush-color-map 'pastel1))

(define theme-continuous
  (theme-override theme-default #:color-map 'cb-bupu-9))

(define (theme->alist theme)
  (map cons
       (list plot-foreground plot-foreground-alpha
             plot-background plot-background-alpha
             plot-font-size plot-font-face plot-font-family
             plot-pen-color-map plot-brush-color-map)
       (struct->list theme)))

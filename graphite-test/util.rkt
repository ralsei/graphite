#lang racket
(require (except-in plot/no-gui points density lines error-bars)
         pict racket/draw rackunit graphite
         file/gzip file/gunzip)
(provide check-same-draw-steps? check-draw-steps mock-record-dc%)

; NOTE: this is adapted almost wholesale from the test suite for plot, except it uses picts
; https://raw.githubusercontent.com/racket/plot/master/plot-test/plot/tests/helpers.rkt

; This component of Racket is distributed under the under the Apache 2.0
; and MIT licenses. The user can choose the license under which they
; will be using the software. There may be other licenses within the
; distribution with which the user must also comply.

; See the files
;   https://github.com/racket/racket/blob/master/racket/src/LICENSE-APACHE.txt)
; and
;   https://github.com/racket/racket/blob/master/racket/src/LICENSE-MIT.txt)
; for the full text of the licenses.

; usually these are specified with keyword arguments, but we want this to apply to all runs
; XXX: change this if we ever stop defaulting to plot params
(plot-width 1024)
(plot-height 768)

; a record-dc% to record the draw commands issued, overriding font options to return constants
; to avoid system discrepancies
(define mock-record-dc%
  (class record-dc%
    (init)
    (super-new)
    (define/override (get-text-extent text [font #f] [combine? #f] [offset 0])
      (values (* 10 (string-length text)) 10 0 0))
    (define/override (get-char-width) 10)
    (define/override (get-char-height) 10)))

; generates the draw steps required to draw the given pict
(define (generate-pict-steps pict)
  (define dc (new mock-record-dc% [width (plot-width)] [height (plot-height)]))
  (draw-pict pict dc 0 0)
  (send dc get-recorded-datum))

; recursively traverses two sets of draw steps, checking for equality
; upon encountering a number, check it in an epsilon of 1e-4
(define (same-draw-steps? set1 set2)
  (match* (set1 set2)
    [((cons first1 rest1) (cons first2 rest2))
     (and (same-draw-steps? first1 first2)
          (same-draw-steps? rest1 rest2))]
    [((? number? a) (? number? b))
     (define diff (abs (- a b))) ; could be inexact
     (< diff 1e-4)]
    [(_ _) (equal? set1 set2)]))

; writes draw steps to a given file, in case the test result changes or does not exist
(define (write-new-draw-steps new-draw-steps original-file)
  (define-values (base name must-be-dir?) (split-path original-file))
  (make-directory* base)                ; ensure the base path exists
  (define data-file (build-path
                     base
                     (string-append "new-" (path->string name))))
  (define data (call-with-output-string (lambda (out) (write new-draw-steps out))))
  (call-with-input-string
   data
   (lambda (in)
     (call-with-output-file
       data-file
       (lambda (out)
         (gzip-through-ports in out #f (current-seconds)))
       #:exists 'replace))))

; reads draw steps from a file
(define (read-draw-steps steps-file)
  (if (file-exists? steps-file)
      (let ()
        (define data
          (call-with-output-string
           (lambda (out)
             (call-with-input-file
               steps-file
               (lambda (in)
                 (gunzip-through-ports in out))))))
        (call-with-input-string data read))
      null))

; checks the draw steps required by the given pict to be the same as that in the file
(define-check (check-draw-steps graph-pict saved-steps-file)
  (define saved (read-draw-steps saved-steps-file))
  (define current (generate-pict-steps graph-pict))
  (unless (same-draw-steps? saved current)
    (write-new-draw-steps current saved-steps-file)
    (define-values (base name must-be-dir?) (split-path saved-steps-file))
    (define data-file (build-path
                       base
                       (string-append "new-" (path->string name))))
    ;; Also generate an image of the current plot
    (define image-file (path-replace-extension data-file ".png"))
    (save-pict graph-pict image-file)
    (fail-check (format "draw steps not the same, new set written to ~a" data-file))))

(define-check (check-same-draw-steps? steps1 steps2)
  (define saved1 (read-draw-steps steps1))
  (define saved2 (read-draw-steps steps2))
  (unless (same-draw-steps? saved1 saved2)
    (fail-check "draw steps not the same")))

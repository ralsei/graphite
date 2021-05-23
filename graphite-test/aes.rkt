#lang racket
(require graphite rackunit)

(module+ test
  (check-equal? (aes #:x "x" #:y "y" #:z "z" #:a "b" #:b "c" #:c "a")
                (hash 'x "x" 'y "y" 'z "z" 'a "b" 'b "c" 'c "a"))

  (check-true (aes? (aes #:x "x" #:y "y" #:z "z")))
  (check-true (not (aes? (make-hasheq))))

  (check-true ((aes-with/c #:x (or/c string? #f)
                           #:y (or/c string? #f)
                           #:z positive-integer?)
               (aes #:x #f #:y "hi" #:z 3)))
  (check-true (not ((aes-with/c #:x (or/c string? #f)
                                #:y (or/c string? #f))
                    (aes #:x #f #:y 3))))
  (check-true (not ((aes-with/c #:x (or/c string? #f)
                                #:y (or/c string? #f)
                                #:z positive-integer?)
                    (aes #:x #f #:y #f))))

  (check-true ((aes-containing/c #:x (or/c string? #f)
                                 #:y (or/c string? #f)
                                 #:z positive-integer?)
               (aes #:x #f #:y "hi" #:z 3)))
  (check-true (not ((aes-containing/c #:x string?)
                    (aes #:x 3))))
  (check-true ((aes-containing/c #:x (or/c string? #f)
                                 #:y (or/c string? #f)
                                 #:z positive-integer?)
               (aes #:z 3)))

  (check-equal? (mapping-override (aes #:x "x" #:y "y" #:z "z")  ; global-mapping
                                  (aes #:y "q" #:z "s" #:a "b")) ; local-mapping
                (aes #:x "x" #:y "q" #:z "s" #:a "b"))
  (check-equal? (mapping-override (aes #:x "x" #:y "y" #:z "z") (aes))
                (aes #:x "x" #:y "y" #:z "z"))
  (check-equal? (mapping-override (aes) (aes #:x "x" #:y "y" #:z "z"))
                (aes #:x "x" #:y "y" #:z "z")))

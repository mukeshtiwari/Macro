#lang racket

; Great job Greg in explaining macros.

(define-syntax foo
  (lambda (stx)
    (syntax "I am mukesh")))


(define-syntax (also-foo stx)
  (syntax "I am also Mukesh"))

(define-syntax (quoted-foo stx)
  #'"I am also Mukesh, but using #' instead of syntax")


(define-syntax (say-hi stx)
  (syntax (display "hello macro world")))

(define-syntax (show-me stx)
  (print stx)
  (syntax (void)))

(define stx (syntax (if x (list "true") #f)))

(syntax-source stx)
(syntax-line stx)
(syntax-column stx)

; Truns syntax object into datum
(syntax->datum stx)
(syntax-e stx)
(syntax->list stx)

(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(reverse-me "backwards" "am" "i" values)

; See the behaviour of if
(if (displayln "true") (displayln "true") (displayln "false"))



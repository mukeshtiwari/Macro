#lang racket


(define-struct nand-gate
  (xin yin) #:transparent)

(define-syntax not-gate
  (syntax-rules ()
   [(not-gate e) (nand-gate e e)]))

(define-syntax and-gate
  (syntax-rules ()
    [(and-gate f-expr s-expr)
     (nand-gate (nand-gate f-expr s-expr)
                (nand-gate f-expr s-expr))]))

(define-syntax or-gate
  (syntax-rules ()
    [(or-gate f-expr s-expr)
     (nand-gate (nand-gate f-expr f-expr)
                (nand-gate s-expr s-expr))]))

(define-syntax nor-gate
  (syntax-rules ()
    [(nor-gate f-expr s-expr)
     (not-gate (or-gate f-expr s-expr))]))

(define-syntax xor-gate
  (syntax-rules ()
    [(xor-gate f-expr s-expr)
     (nand-gate (nand-gate f-expr (nand-gate f-expr s-expr))
                (nand-gate s-expr (nand-gate f-expr s-expr)))]))


(define-syntax xnor-gate
  (syntax-rules ()
    [(xnor-gate f-expr s-expr)
     (nand-gate (nand-gate (nand-gate f-expr f-expr)
                           (nand-gate s-expr s-expr))
                (nand-gate f-expr s-expr))]))
     
;Now just write one evaluator, and you have everything.
; The idea is to keep core simple and checkable. I am wondering
; if this whole thing can be applied in theorem proving.

(define (eval-gate-expr expr)
  (match expr
    [(nand-gate #f #f) #t]
    [(nand-gate #f #t) #t]
    [(nand-gate #t #f) #t]
    [(nand-gate #t #t) #f]
    [(nand-gate f-expr s-expr)
     (let ([feval (eval-gate-expr f-expr)]
           [seval (eval-gate-expr s-expr)])
       (eval-gate-expr (nand-gate feval seval)))]))

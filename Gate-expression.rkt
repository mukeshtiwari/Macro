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

(define-syntax mux
  (syntax-rules ()
    [(mux xin yin sel)
     (nand-gate (nand-gate xin (nand-gate sel sel))
                (nand-gate yin sel))]))

(define-struct wire
  (sout cout) #:transparent)
     
(define-syntax half-adder
  (syntax-rules ()
    [(half-adder xin yin)
     (let ([sout (xor-gate xin yin)]
           [cout (and-gate xin yin)])
       (wire cout sout))])) ;; (wire carry-flag output)



; https://www.elprocus.com/half-adder-and-full-adder/
(define-syntax full-adder
  (syntax-rules ()
    [(full-adder xin yin cin)
     (match-let* ([(wire first-cout first-sout)
                   (half-adder xin yin)]
                  [(wire second-cout final-sout)
                   (half-adder cin first-sout)]
                  [final-cout (or-gate first-cout second-cout)])
       (wire final-cout final-sout))]))
 

; this is going to be bit challenge. Compose half-adder with full adder
; two bit adder
(define-syntax two-bit-adder
  (syntax-rules ()
    [(two-bit-adder (wire (_ x1 x0) (_ y1 y0)))
     (match-let* ([(wire c1 s1) (half-adder x0 y0)]
                  [(wire c2 s2) (full-adder x1 y1 s1)])
       (wire c2 (wire s2 s1)))]))
         
;Now just write one evaluator, and you have everything.
; The idea is to keep core simple and checkable. I am wondering
; if this whole thing can be applied in theorem proving.

(define (eval-gate-expr expr)
  (match expr
    [#t #t]
    [#f #f]
    [(nand-gate #f #f) #t]
    [(nand-gate #f #t) #t]
    [(nand-gate #t #f) #t]
    [(nand-gate #t #t) #f]
    [(nand-gate f-expr s-expr)
     (let ([feval (eval-gate-expr f-expr)]
           [seval (eval-gate-expr s-expr)])
       (eval-gate-expr (nand-gate feval seval)))]))

(define (eval-wire-expr expr)
  (match expr
    [(wire f-expr s-expr)
     (let ([fval (eval-gate-expr f-expr)]
           [sval (eval-gate-expr s-expr)])
       (wire fval sval))]))


(define full-adder-case
  (list (full-adder #f #f #f) (full-adder #f #f #t)
        (full-adder #f #t #f) (full-adder #f #t #t)
        (full-adder #t #f #f) (full-adder #t #f #t)
        (full-adder #t #t #f) (full-adder #t #t #t)))

(map eval-wire-expr full-adder-case)

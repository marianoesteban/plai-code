#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(-) (if (= (length sl) 3) (bminusS (parse (second sl)) (parse (third sl))) (uminusS (parse (second sl))))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(test (interp (desugar (parse '3))) 3)
(test (interp (desugar (parse '(+ 1 2)))) 3)
(test (interp (desugar (parse '(* 2 5)))) 10)
(test (interp (desugar (parse '(+ 1 (* 2 3))))) 7)
(test (interp (desugar (parse '(* (+ 2 5) (* 3 7))))) 147)
(test (interp (desugar (parse '(- 5 2)))) 3)
(test (interp (desugar (parse '(- 4 5)))) -1)
(test (interp (desugar (parse '(* (- 4 1) (+ 5 2))))) 21)
(test (interp (desugar (parse '(- (* 4 5) (+ 1 4))))) 15)
(test (interp (desugar (parse '(- 2)))) -2)
(test (interp (desugar (parse '(- 0)))) 0)
(test (interp (desugar (parse '(- -5)))) 5)
(test (interp (desugar (parse '(- (* 3 5))))) -15)
(test (interp (desugar (parse '(- (* (- 1 4) 2))))) 6)
(test (interp (desugar (parse '(- (- 1) (- 5))))) 4)
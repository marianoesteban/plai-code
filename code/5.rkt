#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (c : ExprC) (t : ExprC) (f : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprS
  [numS (n : number)]
  [idS (s : symbol)]
  [appS (fun : symbol) (arg : ExprS)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [ifS (c : ExprS) (t : ExprS) (f : ExprS)])

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define (subst-helper [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst-helper what for a))]
    [plusC (l r) (plusC (subst-helper what for l)
                        (subst-helper what for r))]
    [multC (l r) (plusC (subst-helper what for l)
                        (subst-helper what for r))]
    [ifC (c t f) (ifC (subst-helper what for c)
                      (subst-helper what for t)
                      (subst-helper what for f))]))

(define (subst [what : number] [for : symbol] [in : ExprC]) : ExprC
  (subst-helper (numC what) for in))

(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(-) (if (= (length sl) 3) (bminusS (parse (second sl)) (parse (third sl))) (uminusS (parse (second sl))))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (cond
                 [(= (length sl) 1) (idS (s-exp->symbol (first sl)))]
                 [(= (length sl) 2) (appS (s-exp->symbol (first sl)) (parse (second sl)))]
                 [else (error 'parse "invalid list input")])]))]
    [else (error 'parse "invalid input")]))

(define (desugar [es : ExprS]) : ExprC
  (type-case ExprS es
    [numS (n) (numC n)]
    [idS (s) (idC s)]
    [appS (f a) (appC f (desugar a))]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [ifS (c t f) (ifC (desugar c) (desugar t) (desugar f))]))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst (interp a fds)
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [ifC (c t f) (if (not (equal? (interp c fds) 0)) (interp t fds) (interp f fds))]))

(define fds (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
                  (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                  (fdC 'const5 '_ (numC 5))
                  (fdC 'negation 'x (ifC (idC 'x) (numC 0) (numC 1)))))

(test (interp (desugar (parse '3)) fds) 3)
(test (interp (desugar (parse '(+ 1 2))) fds) 3)
(test (interp (desugar (parse '(* 2 5))) fds) 10)
(test (interp (desugar (parse '(+ 1 (* 2 3)))) fds) 7)
(test (interp (desugar (parse '(* (+ 2 5) (* 3 7)))) fds) 147)
(test (interp (desugar (parse '(- 5 2))) fds) 3)
(test (interp (desugar (parse '(- 4 5))) fds) -1)
(test (interp (desugar (parse '(* (- 4 1) (+ 5 2)))) fds) 21)
(test (interp (desugar (parse '(- (* 4 5) (+ 1 4)))) fds) 15)
(test (interp (desugar (parse '(- 2))) fds) -2)
(test (interp (desugar (parse '(- 0))) fds) 0)
(test (interp (desugar (parse '(- -5))) fds) 5)
(test (interp (desugar (parse '(- (* 3 5)))) fds) -15)
(test (interp (desugar (parse '(- (* (- 1 4) 2)))) fds) 6)
(test (interp (desugar (parse '(- (- 1) (- 5)))) fds) 4)
(test (interp (desugar (parse '(if 3 4 5))) fds) 4)
(test (interp (desugar (parse '(if 0 4 5))) fds) 5)
(test (interp (desugar (parse '(if (- 1 1) (+ 2 3) (* 2 3)))) fds) 6)
(test (interp (desugar (parse '(+ (if (* 1 2) 3 4) (if -1 5 6)))) fds) 8)
(test (interp (desugar (parse '(double 5))) fds) 10)
(test (interp (desugar (parse '(quadruple 3))) fds) 12)
(test (interp (desugar (parse '(const5 2))) fds) 5)
(test (interp (desugar (parse '(double (+ 1 2)))) fds) 6)
(test (interp (desugar (parse '(+ (quadruple -1) (const5 10)))) fds) 1)
(test (interp (desugar (parse '(negation 1))) fds) 0)
(test (interp (desugar (parse '(double (negation 0)))) fds) 2)
(test (interp (desugar (parse '(+ (negation (* 4 0)) 2))) fds) 3)
(test (interp (desugar (parse '(* (double 1) (quadruple (+ 1 2))))) fds) 24)
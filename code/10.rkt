#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (c : ExprC) (t : ExprC) (f : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [objC (ns : (listof symbol)) (es : (listof ExprC))]
  [msgC (o : ExprC) (n : symbol)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [objV (ns : (listof symbol)) (vs : (listof Value))])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define (lookup-msg [name : symbol] [obj : Value]) : Value
  (cond
    [(empty? (objV-ns obj)) (error 'lookup-msg "name not found")]
    [else (cond
            [(symbol=? name (first (objV-ns obj)))
             (first (objV-vs obj))]
            [else (lookup-msg name (objV (rest (objV-ns obj))
                                         (rest (objV-vs obj))))])]))

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argumente was not a number")]))

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define f-value (interp f env)])
                  (interp (closV-body f-value)
                          (extend-env (bind (closV-arg f-value)
                                            (interp a env))
                                      (closV-env f-value))))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [ifC (c t f) (if (not (equal? (interp c env) (numV 0))) (interp t env) (interp f env))]
    [lamC (a b) (closV a b env)]
    [objC (ns es) (objV ns (map (lambda (e)
                                  (interp e env))
                                es))]
    [msgC (o n) (lookup-msg n (interp o env))]))

(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))
(test (interp (appC (lamC 'x (ifC (idC 'x) (numC 0) (numC 1))) (numC 0))
              mt-env)
      (numV 1))

(test (lookup-msg 'y (objV (list 'x 'y 'z)
                        (list
                         (closV 'f (plusC (idC 'f) (numC 1)) mt-env)
                         (closV '_ (multC (numC 3) (numC 4)) mt-env)
                         (closV 'g (multC (idC 'g) (numC 2)) mt-env))))
      (closV '_ (multC (numC 3) (numC 4)) mt-env))
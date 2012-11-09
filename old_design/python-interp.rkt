#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")
(require (typed-in racket [string>? : (string string -> boolean)]))
(require (typed-in racket [string<? : (string string -> boolean)]))
(require (typed-in racket [string>=? : (string string -> boolean)]))
(require (typed-in racket [string<=? : (string string -> boolean)]))

(define (mult-helper current str n)
  (if (zero? n) current (mult-helper (string-append current str) str (- n 1)))
)

(define (interp-env expr env)
  (type-case CExp expr
    [CNum (n) (VNum n)]
    [CStr (s) (VStr s)]
    [CBool (n) (VBool n)]
    [CError (e) (error 'interp (to-string (interp-env e env)))]

    [CIf (i t e) (type-case CVal (interp-env i env)
      [VBool (n) (if (= n 1)
                     (interp-env t env)
                     (interp-env e env))]
      [VNum (n) (if (not (or (= n 0) (= n .0)))
                     (interp-env t env)
                     (interp-env e env))]
      [VStr (n) (if (string=? n "")
                     (interp-env e env)
                     (interp-env t env))] 
      [VNone () (interp-env e env)]
      [else (interp-env t env)])]

    [CId (x) (type-case (optionof CVal) (hash-ref env x)
      [some (v) v]
      [none () (error 'interp "Unbound identifier")])]

    [CLet (x bind body)
      (interp-env body (hash-set env x (interp-env bind env)))]

    [CSeq (e1 e2)
      (begin (interp-env e1 env) (interp-env e2 env))]

    [CApp (fun arges)
     (type-case CVal (interp-env fun env)
       [VClosure (cenv argxs body)
         (local [(define argvs (map (lambda (e) (interp-env e cenv)) arges))]
          (interp-env body (bind-args argxs argvs env)))]
       [else (error 'interp "Not a closure")])]

    [CFunc (args body) (VClosure env args body)] 

    [CPrim1 (prim arg) (python-prim1 prim (interp-env arg env))]
    [CDict (k v) (VDict (map interp k) (map interp v))]
    [CPass () (VPass)]
    [CNone () (VNone)]
    [CComp (op l r) (type-case CExp op
                      [COp (o)
                      (cond
                        [(string=? o "=") (type-case CVal (interp-env l env)
                                         [VBool (n) (type-case CVal (interp-env r env)
                                                      [VBool (m)  (if (= n m) (VBool 1) (VBool 0))]
                                                      [VNum (m)  (if (= n m) (VBool 1) (VBool 0))]
                                                      [else (VBool 0)])]
                                         [VNum (n) (type-case CVal (interp-env r env)
                                                     [VBool (m)  (if (= n m) (VBool 1) (VBool 0))]
                                                     [VNum (m)  (if (= n m) (VBool 1) (VBool 0))]
                                                     [else (VBool 0)])]
                                         [VStr (n) (type-case CVal (interp-env r env)
                                                     [VStr (m)  (if (string=? n m) (VBool 1) (VBool 0))]
                                                     [else (VBool 0)])] 
                                         [VNone () (type-case CVal (interp-env r env)
                                                     [VNone ()  (VBool 1)]
                                                     [else (VBool 0)])]
                                         [else (VBool 0)])]
                        [(string=? o "!=") (type-case CVal (interp-env l env)
                                         [VBool (n) (type-case CVal (interp-env r env)
                                                      [VBool (m)  (if (= n m) (VBool 0) (VBool 1))]
                                                      [VNum (m)  (if (= n m) (VBool 0) (VBool 1))]
                                                      [else (VBool 1)])]
                                         [VNum (n) (type-case CVal (interp-env r env)
                                                     [VBool (m)  (if (= n m) (VBool 0) (VBool 1))]
                                                     [VNum (m)  (if (= n m) (VBool 0) (VBool 1))]
                                                     [else (VBool 1)])]
                                         [VStr (n) (type-case CVal (interp-env r env)
                                                     [VStr (m)  (if (string=? n m) (VBool 0) (VBool 1))]
                                                     [else (VBool 1)])] 
                                         [VNone () (type-case CVal (interp-env r env)
                                                     [VNone ()  (VBool 0)]
                                                     [else (VBool 1)])]
                                         [else (VBool 0)])]
                        [(string=? o ">") (type-case CVal (interp-env l env)
                                         [VBool (n) (type-case CVal (interp-env r env)
                                                      [VBool (m)  (if (> n m) (VBool 1) (VBool 0))]
                                                      [VNum (m)  (if (> n m) (VBool 1) (VBool 0))]
                                                      [else (error 'interp "Incompatible arguments for comparator")])]
                                         [VNum (n) (type-case CVal (interp-env r env)
                                                     [VBool (m)  (if (> n m) (VBool 1) (VBool 0))]
                                                     [VNum (m)  (if (> n m) (VBool 1) (VBool 0))]
                                                     [else (error 'interp "Incompatible arguments for comparator")])]
                                         [VStr (n) (type-case CVal (interp-env r env)
                                                     [VStr (m)  (if (string>? n m) (VBool 1) (VBool 0))]
                                                     [else (error 'interp "Incompatible arguments for comparator")])] 
                                         [else (error 'interp "Incompatible arguments for comparator")])]
                        [(string=? o "<") (type-case CVal (interp-env l env)
                                         [VBool (n) (type-case CVal (interp-env r env)
                                                      [VBool (m)  (if (< n m) (VBool 1) (VBool 0))]
                                                      [VNum (m)  (if (< n m) (VBool 1) (VBool 0))]
                                                      [else (error 'interp "Incompatible arguments for comparator")])]
                                         [VNum (n) (type-case CVal (interp-env r env)
                                                     [VBool (m)  (if (< n m) (VBool 1) (VBool 0))]
                                                     [VNum (m)  (if (< n m) (VBool 1) (VBool 0))]
                                                     [else (error 'interp "Incompatible arguments for comparator")])]
                                         [VStr (n) (type-case CVal (interp-env r env)
                                                     [VStr (m)  (if (string<? n m) (VBool 1) (VBool 0))]
                                                     [else (error 'interp "Incompatible arguments for comparator")])] 
                                         [else (error 'interp "Incompatible arguments for comparator")])]
                     [(string=? o ">=") (type-case CVal (interp-env l env)
                                         [VBool (n) (type-case CVal (interp-env r env)
                                                      [VBool (m)  (if (>= n m) (VBool 1) (VBool 0))]
                                                      [VNum (m)  (if (>= n m) (VBool 1) (VBool 0))]
                                                      [else (error 'interp "Incompatible arguments for comparator")])]
                                         [VNum (n) (type-case CVal (interp-env r env)
                                                     [VBool (m)  (if (>= n m) (VBool 1) (VBool 0))]
                                                     [VNum (m)  (if (>= n m) (VBool 1) (VBool 0))]
                                                     [else (error 'interp "Incompatible arguments for comparator")])]
                                         [VStr (n) (type-case CVal (interp-env r env)
                                                     [VStr (m)  (if (string>=? n m) (VBool 1) (VBool 0))]
                                                     [else (error 'interp "Incompatible arguments for comparator")])] 
                                         [else (error 'interp "Incompatible arguments for comparator")])]
                        [(string=? o "<=") (type-case CVal (interp-env l env)
                                         [VBool (n) (type-case CVal (interp-env r env)
                                                      [VBool (m)  (if (<= n m) (VBool 1) (VBool 0))]
                                                      [VNum (m)  (if (<= n m) (VBool 1) (VBool 0))]
                                                      [else (error 'interp "Incompatible arguments for comparator")])]
                                         [VNum (n) (type-case CVal (interp-env r env)
                                                     [VBool (m)  (if (<= n m) (VBool 1) (VBool 0))]
                                                     [VNum (m)  (if (<= n m) (VBool 1) (VBool 0))]
                                                     [else (error 'interp "Incompatible arguments for comparator")])]
                                         [VStr (n) (type-case CVal (interp-env r env)
                                                     [VStr (m)  (if (string<=? n m) (VBool 1) (VBool 0))]
                                                     [else (error 'interp "Incompatible arguments for comparator")])] 
                                         [else (error 'interp "Incompatible arguments for comparator")])])]
                      [else (error 'interp "CComp requires COp")])]
    [COp (o) (error 'interp "COp should not be outside CComp")]
    [CBinOp (l r op) 
                      (let ([lval (interp-env l env)]);bind lval
                        (let ([rval (interp-env r env)]);bind rval
                          (type-case
                              CVal rval
                                 [VStr (s1) (
                                            type-case
                                               CVal lval
                                                  [VStr (s2) (if (string=? op "Add") (VStr (string-append s2 s1)) (VStr "error"))]
                                                             

                                                  [VNum (n) (if (string=? op "Mult") (VStr (mult-helper "" s1 n)) (VStr "error"))];string / number
                                                  [VBool (n) (if (string=? op "Mult") (VStr (mult-helper "" s1 n)) (VStr "error"))];string / bool
                                                  [else (error 'interp "unhandled operand for lefthand")]
                                            )]
                                 [VNum (n1) (
                                            type-case
                                               CVal lval
                                                  [VNum (n2) (;number / number
                                                              cond
                                                               [(string=? op "Add") (VNum (+ n1 n2))]
                                                               [(string=? op "Mult") (VNum (* n1 n2))]
                                                               [else (VStr "error: unhandled binop for numbers")]
                                                              )]
                                                  [VBool (n2) (;number / bool
                                                               cond
                                                               [(string=? op "Add") (VNum (+ n1 n2))]
                                                               [(string=? op "Mult") (VNum (* n1 n2))]
                                                               [else (VStr "error: unhandled binop for numbers")]
                                                               )]
                                                  [VStr (s) (;number / string
                                                             if (string=? op "Mult") (VStr (mult-helper "" s n1)) (VStr "error")
                                                             )]
                                                  [else (error 'interp "unhandled operand for lefthand")]
                                            )]
                                 [VBool (n1) (;boolean, can just treat like a number
                                             type-case
                                               CVal lval
                                                  [VNum (n2) (;number / number
                                                              cond
                                                               [(string=? op "Add") (VNum (+ n1 n2))]
                                                               [(string=? op "Mult") (VNum (* n1 n2))]
                                                               [else (VStr "error: unhandled binop for numbers")]
                                                              )]
                                                  [VBool (n2) (;number / bool
                                                               cond
                                                               [(string=? op "Add") (VNum (+ n1 n2))]
                                                               [(string=? op "Mult") (VNum (* n1 n2))]
                                                               [else (VStr "error: unhandled binop for numbers")]
                                                               )]
                                                  [VStr (s) (;number / string
                                                             if (string=? op "Mult") (VStr (mult-helper "" s n1)) (VStr "error")
                                                             )]
                                                  [else (error 'interp "unhandled operand for lefthand")]
                                            )]
                                 [else (error 'interp "unhandled operand for righthand")]
                            )
                            ))]
    ))

(define (bind-args args vals env)
  (cond [(and (empty? args) (empty? vals)) env]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (hash-set (bind-args (rest args) (rest vals) env)
                   (first args) (first vals))]))

(define (interp expr)
  (interp-env expr (hash (list))))


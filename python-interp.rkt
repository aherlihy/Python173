#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

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
    [CPass () (VPass)]
    [CNone () (VNone)]
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


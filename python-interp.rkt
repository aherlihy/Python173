#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

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


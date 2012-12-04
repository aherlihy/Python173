#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(print-only-errors true)

(define (uniq (l : (listof 'a))) : (listof 'a)
  (hash-keys
   (foldl (lambda (x h) (hash-set h x #t))
          (hash empty)
          l)))
(test (uniq empty) empty)
(test (uniq (list 1 2 3 4 3 5 2 3))
      (list 1 2 3 4 5))

(define (find-locals exp)
  (type-case PyExp exp
    [PySet! (id value) (list id)]
    [PySeq (es) (foldl (lambda (exp res)
                         (append (find-locals exp)
                                 res))
                       empty
                       es)]
    [PyIf (test t e)
          (append (find-locals t)
                  (find-locals e))]
    [else empty]))

(define (desugar-inner exp)
  (type-case PyExp exp
    [PyNum (n) (CNum n)]
    [PyTuple (l) (CTuple (map desugar-inner l))] ;~recur on each element in the tuple
    [PySeq (es) (foldl (lambda (e1 e2)
                         (CSeq e2 (desugar-inner e1)))
                       (desugar-inner (first es))
                       (rest es))];~create nested lists since each item in seq is a pair
    [PyId (x) (CId x)]
    [PySet! (id value) (CSet! id (desugar-inner value))]
    [PyApp (f args varargs) 
           ;(case f
            ; [(PyId 'bool) (desugar-inner (PyIf (first args) (PyId 'True) (PyId 'False)))]
            ; [else 
           (CApp (desugar-inner f)
                                  (map desugar-inner args)
                                  (desugar-inner varargs))
          ; ])
           ]
    [PyFunc (args vararg body) (CFunc args vararg (desugar-body body))]
    [PyReturn (value) (CReturn (desugar-inner value))]
    [PyIf (test t e)
          (CLet 'test-value (desugar-inner test)      ; ~tv = desugar(test)
                (CIf (get-and-call (PyId 'test-value) ; app = CApp(tv, "__bool__", empty, {})
                                   "__bool__"
                                   empty
                                   (PyTuple empty))
                     (desugar-inner t)
                     (desugar-inner e)))]             ;~return CIf(app, desugar(t), desugar(e))
    [PyOp (id args)
          (case id
            [(Add) (binop "__add__" (first args) (second args))];(11/4)This is where we want to branch on what kind of args
            [(Sub) (binop "__sub__" (first args) (second args))]
            [(Mult) (binop "__mult__" (first args) (second args))];(11/4)
            [(Div) (binop "__div__" (first args) (second args))];(11/4)
            [(USub) (unop "__neg__" (first args))]
            [(And) (if (= 1 (length args)) (desugar-inner(first args)) (desugar-inner (PyIf (first args) (PyOp id (rest args)) (first args))))];NOTE need to desugar numbers into booleans for numbers to work
            [(Or) (if (= 1 (length args)) (desugar-inner(first args)) (desugar-inner (PyIf (first args) (first args) (PyOp id (rest args)) )))]
            [(Not) (desugar-inner (PyIf (first args) (PyId 'False) (PyId 'True)))]
            [(Gt) (binop ">" (first args) (second args))]
            [(Lt) (desugar-inner (PyOp 'Not (list (PyOp 'GtE args))))]
            [(GtE) (binop ">=" (first args) (second args))]
            [(LtE) (desugar-inner (PyOp 'Not (list (PyOp 'Gt args))))]
            [(Eq) (binop "=" (first args) (second args))]
            [(NotEq) (desugar-inner (PyOp 'Not (list (PyOp 'Eq args))))]
            [(Is) (binop "is" (first args) (second args))]           
            [else (CApp (CPrimF id);~why desugar if not add/sub/etc?
                        (map desugar-inner args)
                        (CTuple empty))])]
    [PyComp (ops l c)
            (if (= 1 (length ops))
                (desugar-inner (PyOp (first ops) (list l (first c))))
                (desugar-inner (PyOp 'And (list (PyOp (first ops) (list l (first c))) (PyComp (rest ops) (first c) (rest c))))))]
    [PyStr (s) (CStr s)]
    [PyPass () (CNone)]
    [PyTryFinal (try final) (CTryFinal (desugar-inner try) (desugar-inner final))]
    [PyTryExcp (try except e) (type-case PyExp except
                                   [PyExcept (t b) (CTryExcp (desugar-inner try) (symbol->string (PyId-x t)) (desugar-inner b) (desugar-inner e))]
                                   [else (error 'desugar "Not handler type in Try Except \n")])]
    [PyRaise (e m) (CRaise e m)]
    ;;[else (error 'desugar (string-append "not implemented: "
    ;;                                     (to-string exp)))]
    [PyExcept (type body) (error 'desugar "Misplaced handler type \n")]
    ))
;returns a CApp
;   takes in (1) Cexp func
;            (2) listof Cexp arglist
;            (3) listof Cexp varargs 
(define (get-and-call inner name args vararg);~what does this do?
  (CApp (CApp (CPrimF 'class-lookup) ;example of instantiating/lookup of an object -> 
                                     ;    type of inner CApp is 'class_lookup', so applies function class lookup
                                     ;    name of param is 'name' passed into func (to be applied correctly, corresponding param needs to come from caller)
                                     ;    pass list of 
              (list (desugar-inner inner)
                    (CStr name))
              (CTuple empty))
        (map desugar-inner args)
        (desugar-inner vararg)))

(define (binop name left right)
  (get-and-call left name
                (list right)
                (PyTuple empty)))

(define (unop name left)
  (CApp (CApp (CPrimF 'class-lookup)
              (list (desugar-inner left)
                    (CStr name))
              (CTuple empty))
        empty
        (CTuple empty)))

(define (desugar-body exp)
  (foldl (lambda (id e)
           (CLet id (CUndefined)
                 e))
         (desugar-inner exp)
         (uniq (find-locals exp))))

(define desugar desugar-body)
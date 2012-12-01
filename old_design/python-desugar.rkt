#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar expr)
  (type-case PyExpr expr
    [PySeq (es) (if (empty? es) (CPass) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es)))]
    [PyNum (n) (CNum n)]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    [PyBool (n) (CBool n)]
    [PyIf (i t e) (CIf (desugar i) (desugar t) (desugar e))]
    [PyStr (s) (CStr s)]
    [PyPass () (CPass)]
    [PyNone () (CNone)]
    [PyRaise (l) (CPass)]
    [PyUnOp (opand op) (CIf (desugar opand) (CBool 0) (CBool 1))]
    [PyBoolOp (vals op) (let ((f (desugar (first vals))))
                         (if (> (length vals) 1)
                        (if (equal? "And" op) 
                            (CIf f
                                 (desugar (PyBoolOp (rest vals) op))
                                 f
                                 )
                            (CIf f
                                 f
                                 (desugar (PyBoolOp (rest vals) op))))
                             f))
    ]
    [PyBinOp (r l op) (CBinOp (desugar l) (desugar r) op)]
    [PyDict (k v) (CDict (map desugar k) (map desugar v))]
    [PyComp (ops comps l) (if (= 1 (length comps)) 
                              (CComp (desugar (first ops)) (desugar l) (desugar (first comps)))
                              (CIf (CComp (desugar (first ops)) (desugar l) (desugar (first comps)))
                                   (desugar (PyComp (rest ops) (rest comps) (first comps)))
                                   (CBool 0)))]
    [PyOp (op) (COp op)]
   ))

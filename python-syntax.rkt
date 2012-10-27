#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyId (x : symbol)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  [PyBool (n : number)]
  [PyIf (i : PyExpr) (t : PyExpr) (e : PyExpr)])


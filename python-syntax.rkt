#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyId (x : symbol)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  [PyBool (n : number)]
  [PyIf (i : PyExpr) (t : PyExpr) (e : PyExpr)]
  [PyStr (s : string)]
  [PyUnOp (opand : PyExpr) (op : string)]
  [PyBinOp (v : (listof PyExpr)) (op : string)]
  [PyPass]
  [PyNone]
  )


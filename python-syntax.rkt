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
  [PyBoolOp (v : (listof PyExpr)) (op : string)]
  [PyBinOp (r : PyExpr) (l : PyExpr) (op : string)]
  [PyPass]
  [PyNone]
  [PyRaise (l : PyExpr)]
  [PyDict (k : (listof PyExpr)) (v : (listof PyExpr))]
  [PyComp (ops : (listof PyExpr)) (comps : (listof PyExpr)) (left : PyExpr)]
  [PyOp (op : string)]
  )


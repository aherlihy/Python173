#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CExp
  [CNum (n : number)]
  [CStr (s : string)]
  [CBool (n : number)]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CError (e1 : CExp)]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CId (x : symbol)]
  [CLet (x : symbol) (bind : CExp) (body : CExp)]
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol)) (body : CExp)]
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPass]
  [CNone]
  [CBinOp (l : CExp) (r : CExp) (op : string)]
  )

(define-type CVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VBool (n : number)]
  [VPass]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExp)]
  [VNone]
  ;eventually add vexcpt
  )
(define-type-alias Env (hashof symbol CVal))


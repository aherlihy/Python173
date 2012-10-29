#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

Takes input from parse-python file (JSON) and produces PyExpr (defined in python-syntax) aka AST

Need to expand to include other surface syntax not yet defined in python-syntax

|#

(define (get-structured-python pyjson)
  ;(write pyjson)
  (match pyjson
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PySeq (map get-structured-python expr-list))]
    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (cond
       [(equal? id "True") (PyBool 1)]
       [(equal? id "False") (PyBool 0)]
       [(equal? id "None") (PyNone)]
       ;add case for 'Exception' -> how to get args?
       [else  (PyId (string->symbol id))])]
    
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    [(hash-table ('nodetype "If")
                 ('test i)
                 ('body t)
                 ('orelse e))
     (PyIf (get-structured-python i) (get-structured-python t) (get-structured-python e))]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    [(hash-table ('nodetype "UnaryOp")
                 ('operand opand)
                 ('op op))
            (match op 
                [(hash-table (nodetype "Not"))
                   (PyUnOp (get-structured-python opand) "Not")]
                [_ (display pyjson) (error 'parse "Haven't handled a case yet for UnaryOp")])]
    
    [(hash-table ('nodetype "BoolOp")
                 ('values v)
                 ('op op))
     (match op
       [(hash-table ('nodetype "And"))
        (PyBinOp (map get-structured-python v) "And")]
       [(hash-table ('nodetype "Or"))
        (PyBinOp (map get-structured-python v) "Or")]
       [_ (error 'parse "Haven't handled a case yet in BinOp")])]       
    [(hash-table ('nodetype "Raise")
                 ('cause c) ;ignore
                 ('exc exc))
             (PyRaise (get-structured-python exc))]
    [(hash-table ('nodetype "Dict")
                 ('keys k) 
                 ('values v))
             (PyDict (map get-structured-python k) (map get-structured-python v))]
    [(hash-table ('nodetype "Compare")
                 ('ops ops) 
                 ('comparators c)
                 ('left l))
             (PyComp (map get-structured-python ops) (map get-structured-python c) (get-structured-python l))]
    [(hash-table ('nodetype "Lt"))
     (PyOp "<")]
    [(hash-table ('nodetype "LtE"))
     (PyOp "<=")]
    [(hash-table ('nodetype "Eq"))
     (PyOp "=")]
    [(hash-table ('nodetype "GtE"))
     (PyOp ">=")]
    [(hash-table ('nodetype "Gt"))
     (PyOp ">")]
    [(hash-table ('nodetype "NotEq"))
     (PyOp "!=")]
    [(hash-table ('nodetype "Is"))
     (PyOp "=")]
    [list (PySeq (map get-structured-python pyjson))]
    [_ (display pyjson) (error 'parse "Haven't handled a case yet")]))
    ;;[_ (error 'parse "Haven't handled a case yet")]))


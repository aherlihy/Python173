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
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (cond
       [(equal? id "True") (PyBool 1)]
       [(equal? id "False") (PyBool 0)]
       [else  (PyId (string->symbol id))])]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    [(hash-table ('nodetype "If")
                 ('test i)  
                 ('orelse e)
                 ('body t))
     (PyIf (get-structured-python i) (get-structured-python t) (get-structured-python e))]
    [_ (display pyjson) (error 'parse "Haven't handled a case yet")]))
    ;;[_ (error 'parse "Haven't handled a case yet")]))


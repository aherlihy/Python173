#lang plai-typed

(require "python-core-syntax.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))

(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VPass () "Pass"]
    [VBool (n) (if (= n 1) "True" "False")]
    [VNone () "None"]
    [VDict (k v) "Dictionary"]
    [VClosure (env args body) (error 'prim "Can't print closures yet")]))
  

(define (print arg)
  (display (string-append (pretty arg) "\n")))

(define (python-prim1 op arg)
  (case op
    [(print) (begin (print arg) arg)]))

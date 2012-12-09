#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

(define (get-structured-python pyjson)
  (match pyjson
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PySeq (map get-structured-python expr-list))]
    
    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]
    
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs)
                 ('args args-list)
                 ('func func-expr))
           
            (match func-expr
              [(hash-table ('nodetype "Attribute")
                         ('ctx c)
                         ('attr func)
                         ('value v)) 
                    (PyApp (PyId (string->symbol func))
                           (cons (get-structured-python v) (map get-structured-python args-list))
                           (if (equal? starargs #\nul)
                               (PyTuple empty)
                               (get-structured-python starargs)))]
              [(hash-table ('nodetype "Name") ;;if it's an application of an id, whose ctx is a load node
                         ('ctx (hash-table ('nodetype "Load")))
                         ('id "___assertRaises"))

                    (match (second args-list)
                      [(hash-table ('nodetype "Attribute")
                                   ('ctx c)
                                   ('attr func)
                                   ('value v)) 
                             (PyApp (get-structured-python func-expr)
                                    (list (PyApp (PyId (string->symbol func))
                                                 (cons (get-structured-python v) (map get-structured-python (rest (rest args-list))))
                                                 (PyTuple empty)))
                                    (if (equal? starargs #\nul)
                                        (PyTuple empty)
                                        (get-structured-python starargs)))]
                      [(hash-table ('nodetype "Name")
                                   ('ctx (hash-table ('nodetype "Load")))
                                   ('id id))
                             (PyApp (get-structured-python func-expr)
                                    (list (PyApp (PyId (string->symbol id))
                                                 (map get-structured-python (rest (rest args-list)))
                                                 (PyTuple empty)))
                                    (if (equal? starargs #\nul)
                                        (PyTuple empty)
                                        (get-structured-python starargs)))]
                      [_ (error 'parse "Raise error\n")])
                    ]
              [_ (PyApp (get-structured-python func-expr)
                (if (empty? args-list) (list (PyNum 0)) 
                 (map get-structured-python args-list))
                 (if (equal? starargs #\nul)
                     (PyTuple empty)
                     (get-structured-python starargs)))])
           ]

    [(hash-table ('nodetype "Name")
                 ('ctx (hash-table ('nodetype "Load")))
                 ('id id))
           (cond 
             [(string=? id "list")(PyId (string->symbol "LIST"))]
             [(string=? id "bool")(PyId (string->symbol "BOOL"))]
             [else  (PyId (string->symbol id))])]
    
    [(hash-table ('nodetype "Assign")
                 ('targets vars)
                 ('value value))
           (match (first vars)
             [(hash-table ('nodetype "Name")
                          ('ctx c)
                          ('id id))
              (PySet! (get-structured-python (first vars))
                      (get-structured-python value))]
             [_ (PyAssign (get-structured-python (first vars))
                          (get-structured-python value))])]
    
    [(hash-table ('nodetype "AugAssign")
                 ('target var)
                 ('value value)
                 ('op (hash-table ('nodetype op))))
           (PySet! (get-structured-python var)
                   (PyOp (string->symbol op)
                         (list (PyId (get-structured-python var));deal with this
                               (get-structured-python value))))]
    
    [(hash-table ('nodetype "Name")
                 ('ctx (hash-table ('nodetype "Store")))
                 ('id id))
     (string->symbol id)]
    
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    
    [(hash-table ('nodetype "arguments")
                 ('args args)
                 ('defaults defaults)
                 ('kwargannotation kwan)
                 ('vararg va)
                 ('kwarg kw)
                 ('varargannotation vaa)
                 ('kw_defaults kwd)
                 ('kwonlyargs kwoa))
           (values (map get-structured-python args)
                   (if (equal? va #\nul)
                       (noneF)
                       (someF (string->symbol va))))]
    
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list dl)
                 ('returns ret))
           (local [(define-values (va n-args) (get-structured-python args))]
             (PySet! (string->symbol name)
                     (PyFunc va n-args
                             (PySeq (map get-structured-python body)))))]
    
    [(hash-table ('nodetype "Lambda")
                 ('args args)
                 ('body body))
     (local [(define-values (va n-args) (get-structured-python args))]
       (PyFunc va n-args
               (PyReturn (get-structured-python body))))]
    
    [(hash-table ('nodetype "arg")
                 ('arg id)
                 ('annotation an))
     (string->symbol id)]
    
    [(hash-table ('nodetype "Return")
                 ('value value))
     (PyReturn (get-structured-python value))]
    
    [(hash-table ('nodetype "If")
                 ('test test)
                 ('body body)
                 ('orelse (list)))
     (PyIf (get-structured-python test)
           (PySeq (map get-structured-python body))
           (PyId 'None))]
    
    [(hash-table ('nodetype "If")
                 ('test test)
                 ('body body)
                 ('orelse else))
     (PyIf (get-structured-python test)
           (PySeq (map get-structured-python body))
           (PySeq (map get-structured-python else)))]
    
    [(hash-table ('nodetype "BinOp")
                 ('op (hash-table ('nodetype op)))
                 ('left left)
                 ('right right))
     (PyOp (string->symbol op)
           (list (get-structured-python left)
                 (get-structured-python right)))]
    
    [(hash-table ('nodetype "BoolOp")
                 ('values v)
                 ('op (hash-table ('nodetype op))))
     (PyOp (string->symbol op)
           (map get-structured-python v))]
    
    [(hash-table ('nodetype "UnaryOp")
                 ('op (hash-table ('nodetype op)))
                 ('operand operand))
     (PyOp (string->symbol op)
           (list (get-structured-python operand)))]
    
    [(hash-table ('nodetype "Tuple")
                 ('ctx ctx)
                 ('elts elts))
     (PyTuple
      (map get-structured-python elts))]
    
    [(hash-table ('nodetype "Raise")
                 ('cause c)
                 ('exc e))
     (if (equal? e #\nul)
         (PyRaise "ReRaise" (list))
         (match e
           [(hash-table ('nodetype "Call")
                        ('keywords keywords) ;; ignoring keywords for now
                        ('kwargs kwargs)     ;; ignoring kwargs for now
                        ('starargs starargs)
                        ('args args-list)
                        ('func func-expr)) 
            (match func-expr
              [(hash-table ('nodetype "Name")
                           ('ctx ctx)
                           ('id id))
               (PyRaise id (map (lambda (str)
                                  (match str
                                    [(hash-table ('nodetype "Str")
                                                 ('s s))
                                     s]
                                    [_ (error 'parse "Not a valid exception message \n")]))
                                args-list))]
              [_ (error 'parse "Raise error\n")])]))]
    
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]

    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    
    [(hash-table ('nodetype "Compare")
                 ('ops ops) 
                 ('comparators c)
                 ('left l))
     (PyComp (map string->symbol (map (lambda (x) (hash-ref x 'nodetype))
                                      ops)) 
             (get-structured-python l) 
             (map get-structured-python c))]
    
    [(hash-table ('nodetype "TryFinally")
                 ('body try)
                 ('finalbody final))
     (PyTryFinal (PySeq (map get-structured-python try)) (PySeq (map get-structured-python final)))]
    
    [(hash-table ('nodetype "TryExcept")
                 ('body try)
                 ('orelse e)
                 ('handlers excpt))
     (PyTryExcp (PySeq (map get-structured-python try)) (get-structured-python (first excpt)) (if (empty? e) (PyPass) (PySeq (map get-structured-python e))))]
    
    [(hash-table ('nodetype "ExceptHandler")
                 ('body except)
                 ('name n)
                 ('type type))
     (PyExcept (if (equal? type #\nul) (PyId (string->symbol "ExceptAll")) (get-structured-python type))
               (PySeq (map get-structured-python except)))]
    
    
    [(hash-table ('nodetype "List")
                 ('ctx ctx)
                 ('elts elts))
     (PyList
      (map get-structured-python elts))]
    
    
    [(hash-table ('nodetype "Set")
                 ('elts elts))
     (PyList
      (map get-structured-python elts))]
    
    
    [(hash-table ('nodetype "Dict")
                 ('keys keys)
                 ('values values))
     (PyDict
      (map get-structured-python keys) (map get-structured-python values))]
    
    
    [(hash-table ('nodetype "Subscript")
                 ('slice slice)
                 ('ctx c)
                 ('value dict))
     (match slice
       [(hash-table ('nodetype "Index")
                    ('value arg))
        (match c
          [(hash-table ('nodetype "Store")) (PyDictStore (get-structured-python dict) (get-structured-python arg))]
          [(hash-table ('nodetype "Load"))  
           ; (match dict
           ;   [(hash-table ('nodetype "Dict")
           ;                ('keys keys)
           ;                ('values values))
           (PyDictLoad (get-structured-python dict) (get-structured-python arg))];dict + suc
          ;   [(hash-table ('nodetype "Str")
          ;                ('s s))
          ;    (PyStrLoad (]
          [(hash-table ('nodetype "Del")) (PyOp (string->symbol "del") (list (get-structured-python dict) (get-structured-python arg)))]
          [_ (error 'parse (string-append "Haven't handled non store/load:\n"
                                          (format "~s" pyjson)))])]
       [(hash-table ('nodetype "Slice")
                    ('upper upper)  
                    ('lower lower)
                    ('step step))
        (PySlice (get-structured-python dict) (if (equal? lower #\nul) (PyNum 0) (get-structured-python lower)) (if (equal? upper #\nul) (PyNum 1024) (get-structured-python upper)) (if (equal? step #\nul) (PyNum 1) (get-structured-python step)))]
       [_ (error 'parse (string-append "Haven't handled slicing:\n"
                                       (format "~s" pyjson)))])]
    
    [(hash-table ('nodetype "Delete")
                 ('targets target))
     (get-structured-python (first target))]
    
    
    [_ (error 'parse (string-append "Haven't handled a case yet:\n"
                                    (format "~s" pyjson)))]))


#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")
(require (typed-in racket [string-length : (string -> number)]))

(print-only-errors true)

(define (uniq (l : (listof 'a))) : (listof 'a)
  (hash-keys
   (foldl (lambda (x h) (hash-set h x #t))
          (hash empty)
          l)))
(test (uniq empty) empty)
(test (uniq (list 1 2 3 4 3 5 2 3))
      (list 1 2 3 4 5))

(define global-ignore empty)

(define (find-nonlocals exp)
  (type-case PyExp exp    
    [PyNonLocal (id) (list id)]
    [PySeq (es) (foldl (lambda (exp res)
                         (append (find-nonlocals exp)
                                 res))
                       empty
                       es)]
    [PyIf (test t e)
          (append (find-nonlocals t)
                  (find-nonlocals e))]
    [PyTryExcp (try exep els)(append (find-nonlocals try)
                                      (append (find-nonlocals exep)
                                      (find-nonlocals els)))]
    [PyExcept (type as body) (find-nonlocals body)]
    [PyTryFinal (try fin) (append (find-nonlocals try)
                                (find-nonlocals fin))]
    [else empty]))

(define (find-func-locals exp)
  (type-case PyExp exp
    [PySet! (id value) (append (if (not (member id global-ignore)) 
                           (list id) 
                           empty) (find-func-locals value))]
    [PyId (x) (if (not (member x global-ignore)) 
                           (list x) 
                           empty)] 
    [PySeq (es) (foldl (lambda (exp res)
                         (append (find-func-locals exp)
                                 res))
                       empty
                       es)]
    [PyIf (test t e)
          (append (find-func-locals t)
                  (find-func-locals e))]
    [PyTryExcp (try exep els)(append (find-func-locals try)
                                      (append (find-func-locals exep)
                                      (find-func-locals els)))]
    [PyExcept (type as body) (find-func-locals body)]
    [PyTryFinal (try fin) (append (find-func-locals try)
                                (find-func-locals fin))]
    [PyOp (id arg) (foldr (lambda (x l) (append (find-func-locals x) l)) empty arg)]
    [else empty]))

(define (find-locals exp ignorelist)
  (type-case PyExp exp
    [PySet! (id value) (if (and (not (member id ignorelist)) (not (member id global-ignore))) 
                           (list id) 
                           empty)]
    [PyMultSet! (ids value) ids]
                ;(foldr (lambda (x l) (if (and (not (member x ignorelist)) (not (member x global-ignore)))
                 ;                        (cons x l)
                  ;                       l)) 
                   ;    empty
                    ;   ids)]
    [PySeq (es) (foldl (lambda (exp res)
                         (append (find-locals exp ignorelist)
                                 res))
                       empty
                       es)]
    [PyIf (test t e)
          (append (find-locals t ignorelist)
                  (find-locals e ignorelist))]
    [PyTryExcp (try exep els)(append (find-locals try ignorelist)
                                      (append (find-locals exep ignorelist)
                                      (find-locals els ignorelist)))]
    [PyExcept (type as body) (find-locals body ignorelist)]
    [PyTryFinal (try fin) (append (find-locals try ignorelist)
                                (find-locals fin ignorelist))]
    [PyClassDef (name super body) (if (and (not (member name ignorelist)) (not (member name global-ignore))) 
                           (list name) 
                           empty)]
    [else empty]))

(define (desugar-inner exp locals) 
  (type-case PyExp exp
    [PyNum (n) (CNum n)]
    [PyTuple (l) (CTuple (map (lambda (x) (desugar-inner x locals)) l))] ;~recur on each element in the tuple
    [PySeq (es) (foldl (lambda (e1 e2)
                         (CSeq e2 (desugar-inner e1 locals)))
                       (desugar-inner (first es) locals)
                       (rest es))];~create nested lists since each item in seq is a pair
    [PyId (x) (CId x)]
    [PyNonLocal (x) (CNone)]
    [PyGlobal (x) (begin (set! global-ignore (cons x global-ignore)) (CAddGlobal x (CUndefined)))]
    [PySet! (id value) (CSet! id (desugar-inner value locals) )]
    [PyMultSet! (id value) (CMultSet! id (desugar-inner value locals) )]
    [PyApp (f args varargs) 
           (cond
             [(equal? f (PyId '___assertRaises)) 
              (CApp 
               (desugar-inner f locals)
               (list (CTryExcp (desugar (first args))
                    "ExceptAll"
                    (CNone)
                    (CError (CStr "assertion failed"))
                    (string->symbol "")))
               (desugar-inner varargs locals))]
             [(equal? f (PyId 'locals))
              (CLocals (uniq locals))]
           #|  [(equal? f (PyId 'any))
              (if 
               (= 1 (length args))
              (any-helper args 0)
              (CError (CStr "TypeError")))]|#
             [(equal? f (PyId 'isinstance))
              (if (equal? (length args) 2)
                  (if (PyStr? (second args))
                      (CError (CStr "TypeError: wrong type of args"))
                      (CApp (desugar-inner f locals)
                            (list (desugar-inner (first args) locals) (CStr (symbol->string (PyId-x (second args)))))
                            (desugar-inner varargs locals)))
                  (CError (CStr "TypeError: wrong number of args")))
                  ]
               
           [else (CApp (desugar-inner f locals)
                                  (map (lambda (x) (desugar-inner x locals)) args)
                                  (desugar-inner varargs locals))])
           ]
    [PyCmp (id iter func) (CCmp (desugar-inner iter locals)
                                      (desugar-inner (PyFunc (list id) (none) (PyReturn func)) locals))]#|(CList (map (lambda (elem) 
                                        (CApp 
                                         (CFunc 
                                          (list id) 
                                          (none) 
                                          (CReturn (desugar-body func))) 
                                         (list elem)
                                         (CTuple empty))) 
                                      (CList-elts (desugar-inner iter))))|#
                                              ;;(CApp (CId 'comprehension)(list (desugar-inner id)(desugar-inner iter)(desugar-inner func))(CTuple empty))]
    [PyFunc (args vararg body) (CFunc args vararg (desugar-body body (append args (find-nonlocals body)) (find-func-locals body)))]
    [PyClassDef (name super body) 
                (CClassDef name super (desugar-body (if (PySeq? body)
                                                        (PySeq (append (PySeq-es body) (list (PyReturn (PyApp (PyId 'locals) empty (PyTuple empty))))))
                                                        (PySeq (cons body (list (PyReturn (PyApp (PyId 'locals) empty (PyTuple empty)))))))
                                                    (find-nonlocals body) (find-locals body (find-nonlocals body))))]
    ;[PyScopeWrap (x) (desugar-body x empty)]
    [PyReturn (value) (CReturn (desugar-inner value locals))]
    [PyIf (test t e)
          (CLet 'test-value (desugar-inner test locals)      ; ~tv = desugar(test)
                (CIf (get-and-call (PyId 'test-value) ; app = CApp(tv, "__bool__", empty, {})
                                   "__bool__"
                                   empty
                                   (PyTuple empty) locals)
                     (desugar-inner t locals)
                     (desugar-inner e locals)))]             ;~return CIf(app, desugar(t), desugar(e))
    [PyOp (id args)
          (case id
            [(Add) (binop "__add__" (first args) (second args) locals)];(11/4)This is where we want to branch on what kind of args
            [(Sub) (binop "__sub__" (first args) (second args) locals)]
            [(Mult) (binop "__mult__" (first args) (second args) locals)];(11/4)
            [(FloorDiv) (binop "__div-floor__" (first args) (second args) locals)]
            [(Mod) (binop "__mod__" (first args) (second args) locals)]
            [(Div) (binop "__div__" (first args) (second args) locals)];(11/4)
            [(USub) (unop "__neg__" (first args) locals)]
            [(Invert) (unop "__inv__" (first args) locals)]
            [(UAdd) (unop "__pls__" (first args) locals)]
            [(BitAnd) (binop "__bitand__" (first args) (second args) locals)]
            [(BitOr) (binop "__bitor__" (first args) (second args) locals)]
            [(BitXor) (binop "__bitxor__" (first args) (second args) locals)]
            [(And) (if (= 1 (length args)) (desugar-inner (first args) locals) (desugar-inner (PyIf (first args) (PyOp id (rest args)) (first args)) locals))];NOTE need to desugar numbers into booleans for numbers to work
            [(Or) (if (= 1 (length args)) (desugar-inner (first args) locals) (desugar-inner (PyIf (first args) (first args) (PyOp id (rest args))) locals))]
            [(Not) (desugar-inner (PyIf (first args) (PyId 'False) (PyId 'True)) locals)]
            [(Gt) (binop ">" (first args) (second args) locals)]
            [(Lt) (desugar-inner (PyOp 'Not (list (PyOp 'GtE args))) locals)]
            [(GtE) (binop ">=" (first args) (second args) locals)]
            [(LtE) (desugar-inner (PyOp 'Not (list (PyOp 'Gt args))) locals)]
            [(Eq) (binop "=" (first args) (second args) locals)]
            [(NotEq) (desugar-inner (PyOp 'Not (list (PyOp 'Eq args))) locals)]
            [(Is) (binop "is" (first args) (second args) locals)]   
            [(IsNot) (desugar-inner (PyOp 'Not (list (PyOp 'Is args))) locals)]
            [(In) (binop "in" (second args) (first args) locals)]
            [(NotIn) (desugar-inner (PyOp 'Not (list (PyOp 'In args))) locals)]
            [(del) (binop "del" (first args) (second args) locals)]
            [else (get-and-call-obj (first args) (symbol->string id) (rest args) (PyTuple empty) locals)
             ;(CApp (CPrimF id);~why desugar if not add/sub/etc?
                        ;(map (lambda (x) (desugar-inner x locals)) args)
                        ;(CTuple empty))
             ])]
    [PyComp (ops l c)
            (if (= 1 (length ops))
                (desugar-inner (PyOp (first ops) (list l (first c))) locals)
                (desugar-inner (PyOp 'And (list (PyOp (first ops) (list l (first c))) (PyComp (rest ops) (first c) (rest c)))) locals))]
    [PyStr (s) (CStr s)]
    [PyPass () (CNone)]
    [PyTryFinal (try final) (CTryFinal (desugar-inner try locals) (desugar-inner final locals))]
    [PyTryExcp (try except e) (type-case PyExp except
                                   [PyExcept (t as b) (CTryExcp (desugar-inner try locals) (symbol->string (PyId-x t)) (desugar-inner b locals) (desugar-inner e locals) as)]
                                   [else (error 'desugar "Not handler type in Try Except \n")])]
    [PyRaise (e m) (CRaise e m)]
    ;;[else (error 'desugar (string-append "not implemented: "
    ;;                                     (to-string exp)))]
    [PyExcept (type as body) (error 'desugar "Misplaced handler type \n")]
    [PyList (elts) (CList (map (lambda (x) (desugar-inner x locals)) elts))]
    [PyDict (keys values) (CDictM (CBox (CDict (map (lambda (x) (desugar-inner x locals)) keys) (map (lambda (x) (desugar-inner x locals)) values))))]
    [PyDictLoad (dict key) (CDictLoad (desugar-inner dict locals) (desugar-inner key locals))]
    [PyDictStore (dict key) (CDictStore (desugar-inner dict locals) (desugar-inner key locals))]
    [PyAssign (to from) (CAssign (desugar-inner to locals) (desugar-inner from locals))]
    [PySlice (val lower upper step) (CSlice (desugar-inner val locals) (desugar-inner lower locals) (desugar-inner upper locals) (desugar-inner step locals))]
    [PyFieldLookup (field value) 
                   (CApp (CPrimF 'object-lookup) 
                         (list (desugar-inner value locals)
                               (CStr field))
                         (CTuple empty))]
    ))


;returns a CApp
;   takes in (1) Cexp func
;            (2) listof Cexp arglist
;            (3) listof Cexp varargs 
(define (get-and-call inner name args vararg locals);~what does this do?
  (CApp (CApp (CPrimF 'class-lookup) ;example of instantiating/lookup of an object -> 
                                     ;    type of inner CApp is 'class_lookup', so applies function class lookup
                                     ;    name of param is 'name' passed into func (to be applied correctly, corresponding param needs to come from caller)
                                     ;    pass list of 
              (list (desugar-inner inner locals)
                    (CStr name))
              (CTuple empty))
        (map (lambda (x) (desugar-inner x locals)) args)
        (desugar-inner vararg locals)))

(define (get-and-call-obj inner name args vararg locals);~what does this do?
  (CApp (CApp (CPrimF 'object-lookup) ;example of instantiating/lookup of an object -> 
                                     ;    type of inner CApp is 'class_lookup', so applies function class lookup
                                     ;    name of param is 'name' passed into func (to be applied correctly, corresponding param needs to come from caller)
                                     ;    pass list of 
              (list (desugar-inner inner locals)
                    (CStr name))
              (CTuple empty))
         (map (lambda (x) (desugar-inner x locals)) args)
        (desugar-inner vararg locals)))

(define (binop name left right locals)
  (get-and-call left name
                (list right)
                (PyTuple empty) locals))

#|(define (any-helper args num)
  (let ((l (desugar-inner (first args))))
              (if (or (CList? l) 9CId
                  (if  (= num (length (CList-elts l)))
                       (CFalse)
                       (CIf (CApp (CApp (CPrimF 'class-lookup)
                                        (list (list-ref (CList-elts l) num)
                                        (CStr "__bool__"))
                                  (CTuple empty))
                            empty
                            (CTuple empty))
                       (CTrue)
                       (any-helper args (+ 1 num))))
                  (CError (CStr "TypeError \n"))))))|#

(define (unop name left locals)
  (CApp (CApp (CPrimF 'class-lookup)
              (list (desugar-inner left locals)
                    (CStr name))
              (CTuple empty))
        empty
        (CTuple empty)))

(define (desugar-body exp l locals)
  (foldl (lambda (id e)
           (CLet id (CUndefined)
                 e))
         (desugar-inner exp locals)
         (uniq (find-locals exp l))))

(define (desugar exp)
  (desugar-body exp empty empty))
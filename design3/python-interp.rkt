#lang plai-typed

(require "python-core-syntax.rkt" 
         "python-monad.rkt"
         "python-lib.rkt"
         (typed-in racket/base
                   [display : (string -> void)]
                   [andmap : (('a -> boolean) (listof 'a) -> boolean)])
         (typed-in racket/math))
(require (typed-in racket [eqv? : ('a 'b -> boolean)]))
(require (typed-in racket [string>? : (string string -> boolean)]))
(require (typed-in racket [string>=? : (string string -> boolean)]))
(require (typed-in racket [string<=? : (string string -> boolean)]))
(require (typed-in racket [string->number : (string -> number)]))
(require (typed-in racket [regexp-split : (string string -> (listof string))]))
(require (typed-in racket [remove* : ((listof 'a) (listof 'a) -> (listof 'a))]))
(require (typed-in racket [remove : ('a (listof 'a) -> (listof 'a))]))
(require (typed-in racket [hash-values : ((hashof 'a 'b) -> (listof 'b))]))
(require (typed-in racket [hash-keys : ((hashof 'a 'b) -> (listof 'a))]))
(require (typed-in racket [string-split : (string -> (listof string))]))
(require (typed-in racket [substring : (string number number -> string)]))
(require (typed-in racket [number->string : (number -> string)]))
(require (typed-in racket [hash->list : ((hashof 'a 'a) -> (listof (listof 'a)))]))
(require (typed-in racket [remove-duplicates : ((listof 'a) -> (listof 'a))]))
(require (typed-in racket [abs : (number -> number)]))
(require (typed-in racket [string-length : (string -> number)]))
(require (typed-in racket [min : (number number -> number)]))
(require (typed-in racket [max : (number number -> number)]))
(require (typed-in lang/htdp-advanced
                [string-contains? : (string string -> boolean)]))
(require (typed-in racket/list
                [range : (number -> (listof number))]))


;;note: interp and primitives need to be mutually recursive -- primops
;;need to lookup and apply underscore members (ie + calls __plus__),
;;and applying a function requires m-interp.  Since racket doesn't
;;allow mutually recursive modules, I had to move python-primitives
;;into python-interp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Primitives          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;note: primitive functions have type (listof CVal) -> (PM CVal)
;;they take in a list of vals because they may take in an arbitrary
;;number of values (think print).  At some point, I will add in better
;;argument checking to prim functions, but at the moment, I just
;;assume they are correct


(define last_raise (CRaise "RuntimeError" (list "No active exception"))) ; keep track of prev raise for re-raises


;;helper macro for (define-primf)
(define-syntax prim-bind
  (syntax-rules (&)
    [(prim-bind val (& (bind pred)) body)
     (let ([bind val])
       (if (pred bind)
           body
           (interp-error "primf given wrong argument type")))]
    [(prim-bind val (& bind) body)
     (let ([bind val])
       body)]
    [(prim-bind val ((bind pred) binds ...) body)
     (let ([val-val val])
       (if (empty? val-val)
           (interp-error "primf not given enough arguments")
           (let ([bind (first val-val)])
             (if (pred bind)
                 (prim-bind (rest val-val) (binds ...) body)
                 (interp-error "primf given wrong argument type")))))]
    [(prim-bind val (bind binds ...) body)
            (let ([val-val val])
       (if (empty? val-val)
           (let ([bind (VNone)])
             (prim-bind empty (binds ...) body))
           ;(interp-error "primf not given enough arguments2")
           (let ([bind (first val-val)])
             (prim-bind (rest val-val) (binds ...) body))))]
    [(prim-bind val () body)
     (if (empty? val)
         body
         (interp-error "primf given too many arguments"))]))

;;defines the racket version of a primf.
;;syntax: (define-primf (name . args) body)
;;args: (arg ... & rest)
;;arg (including rest): id or (id predicate)[(is) is]
;;notes: & rest is optional, bound to remaining arguments
;;actual racket function's type is ((listof CVal) -> (PM CVal))
;;if predicate is present, define-primf throws an error unless the
;;predicate returns true when applied to its associated arg
(define-syntax define-primf
  (syntax-rules () ;~basically pattern matching
    [(define-primf (name . args) body);~ . generates a pair, so primf is given a pair (name+args) and then a body.
     (define (name (arg : (listof CVal))) : (PM CVal)
       (prim-bind arg args body))]))

;;get the class of obj
(define-primf (class obj)
  (type-case CVal obj
    [VUndefined () (interp-error "local used before being set")]
    [VNone () (get-global "none-type")]
    [VBool (n) (get-global "bool-type")]
    [VNum (n) (get-global "num-type")]
    [VStr (s) (get-global "str-type")]
    [VBox (v) (interp-error "Boxes don't have a class")]
    [VObj (dict class) (get-box (list class))]
    [VPrimF (id) (get-global "func-type")]
    [VPrimMap (m) (interp-error "prim maps don't have a class")]
    [VClosure (e a v b) (get-global "func-type")]
    [VTuple (l) (get-global "tuple-type")]
    [VList (elts) (get-global "list-type")]
    [VDict (h) (get-global "dict-type")]
    [VDictM (h) (get-global "mutable-dict-type")]))

;;get the dict out of obj
(define (dict (obj : CVal)) : (PM CVal)
  (type-case CVal obj
    [VObj (dict class) (get-box (list dict))]
    [else (m-return (VNone))]))

;;get the super class of the class c (c's class must be type)
(define (super (c : CVal)) : (PM CVal)
  (m-do ([c-c (class (list c))]
         [class-type (get-global "class-type")]
         [(if (eq? c-c class-type)
              (pm-catch-error
               (local-lookup c (VStr "__super__"))
               (lambda (error)
                 (get-global "obj-type")))
              (m-do ([c-s (pretty c)]
                     [(interp-error (string-append "isn't a class:"
                                                   c-s))])))])))

;;add a value to a prim dict (return the new dict)
(define (prim-dict-add (c-dict : CVal) (key : CVal) (val : CVal)) : (PM CVal)
  (type-case CVal c-dict
    [VPrimMap (m)
              (m-return (VPrimMap (hash-set m key val)))]
    [else (interp-error "prim-dict-add expects a prim-dict")]))

;;lookup a value in a prim dict
(define (prim-dict-lookup (c-dict : CVal) (key : CVal)) : (PM CVal)
  (type-case CVal c-dict
    [VPrimMap (m) 
              (type-case (optionof CVal) (hash-ref m key)
                [some (v) (m-return v)]
                [none () (interp-error
                                 (string-append "key not found: "
                                         (VStr-s key)))])];why would be passed __add__
    [else (interp-error
           (string-append "prim-dict-lookup wants a dict: "
                          (to-string key)))]))


  


;;lookup a value in an object (fails if there is no dict)
(define (local-lookup (obj : CVal) (key : CVal)) : (PM CVal)
  (m-do ([c-dict (dict obj)]
         [(prim-dict-lookup c-dict key)])))

;;takes an object in the first position and a key in the second
;;position, and returns true iff the object's class (or its
;;superclasses) contains key
(define-primf (class-has-member? & args)
  (pm-catch-error (m-do ([(class-lookup args)])
                        (VBool 1))
                  (lambda (x)
                    (m-return (VBool 0)))))

;;takes an object in the first position and a key in the second
;;position, returns the value that the class has for key. Errors if
;;the key is not present in the class or its superclasses.  Function
;;return values are curried with the object (so if the class contains
;;(lambda (this) this), the returned function is equivalent to (lamba ()
;;this), where this refers to the object.  This sets up obj.method()
;;semantics properly
(define-primf (class-lookup object name)
  (m-do
   ([obj-type (get-global "obj-type")]
    [partial (get-global "partial-apply")]
    [(local [(define (iter c)
               (m-do ([c-dict (dict c)]
                      [(pm-catch-error
                        (local-lookup c name)
                        (lambda (error)
                          (if (eq? c obj-type)
                              (pm-error error)
                              (m-bind (super c)
                                      iter))))])))]
            (m-do ([c (class (list object))]
                   [res (iter c)]
                   [(if (or (VClosure? res)
                            (VPrimF? res))
                        (apply-func partial
                                    (list res object)
                                    (VTuple empty))
                        (m-return res))])))])))

;;take an object in the first position and a key in the second
;;position.  looks the key up in the objects dict first, and in the
;;objects class second (if the key isn't in the dict)
(define-primf (obj-lookup obj name)
  (cond
     [(equal? name (VStr "__dict__")) (dict obj)]
     [(equal? name (VStr "__class__")) (class (list obj))]
     [else (pm-catch-error
            (local-lookup obj name)
            (lambda (error)
              (class-lookup (list obj name))))]))


;;will be replaced by a to-string method in classes
(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (m-return (to-string n))]
    [VNone () (m-return "None")]
    [VStr (s) (m-return s)]
    [VTuple (l) (m-do ([vals (m-map pretty l)]
                       [rvals (m-return (reverse vals))])
                      (cond
                       [(empty? rvals) "()"]
                       [(empty? (rest rvals))
                        (string-append "("
                                       (string-append (first rvals)
                                                      ",)"))]
                       [else (string-append
                              "("
                              (string-append
                               (foldl (lambda (c t)
                                        (string-append c
                                                       (string-append ", "
                                                                      t)))
                                      (first rvals)
                                      (rest rvals))
                               ")"))]))]
    [VList (l) (m-do ([vals (m-map pretty l)]
                       [rvals (m-return (reverse vals))])
                      (cond
                       [(empty? rvals) "[]"]
                       [(empty? (rest rvals))
                        (string-append "["
                                       (string-append (first rvals)
                                                      ",]"))]
                       [else (string-append
                              "["
                              (string-append
                               (foldl (lambda (c t)
                                        (string-append c
                                                       (string-append ", "
                                                                      t)))
                                      (first rvals)
                                      (rest rvals))
                               "]"))]))]
    [VDictM (h) (m-do ([dict (get-box (list h))]
                       [toPrint (pretty (VList (foldr cons (hash-values (VDict-hashes dict)) (hash-keys (VDict-hashes dict)))))])
                toPrint)]
    [VObj (c dict)
          (m-do ([c (get-box (list c))]
                 [c-s (pretty c)]
                 [dict (get-box (list dict))]
                 [dict-s (pretty dict)])
                (string-append "(obj "
                               (string-append c-s
                                              (string-append " "
                                                             (string-append dict-s
                                                                            ")")))))]
    [else (m-return (to-string arg))]))



;;gets the global variable dict
(define get-globals
  (pm-lookup-store -2))

;;sets the global variable dict
(define (set-globals (v : CVal))
  (type-case CVal v
    [VPrimMap (m) (pm-add-store -2 v)]
    [else (interp-error "globals must be a prim dict")]))

;;gets a particular global from the global varaible dict
(define (get-global (arg : string))
  (m-do ([d get-globals]
         [(prim-dict-lookup d (VStr arg))])))

;;adds a global variable and its value to the global variable dict
(define (add-global (name : string) (val : CVal))
  (m-do ([d get-globals]
         [new-d (prim-dict-add d (VStr name) val)]
         [(set-globals new-d)])))

;;prints the args, separated by spaces, followed by a newline
(define-primf (print val & rest)
  (m-do ([prettied (pretty val)]
         [(m-return (display prettied))]
         [(if (empty? rest)
              (begin (display "\n")
                     (m-return (VNone)))
              (begin (display " ")
                     (print rest)))])))

;;assert-raises
#|(define-primf (asser-raises raise & rest)
  (pm-try-catch (m-interp raise env) 
                              (lambda (error) ; this will  be called if an error has been thrown and not caught
                                    (m-interp (CError (CStr "assertion failed")) env)) ;;(interp-error (VStr-s error)))))
                              (lambda (x) (m-interp CNone env))
                              )
  )|#

;get item from hashmap
(define-primf (get val & ret);first val is attr, ret is a list of actual args
    (if (empty? ret)
        (interp-error "TypeError")
        (m-do ([contents (get-box (list (VDictM-b val)))]
          [(type-case (optionof CVal) (hash-ref (VDict-hashes contents) (first ret))
            [some (v) (m-return v)]
            [none () (cond
                       [(equal? 1 (length ret)) (m-return (VNone))]
                       [(equal? 2 (length ret)) (m-return (second ret))]
                       [else (interp-error "TypeError")])])]))))

(define-primf (get-tuple-val val & args)
  (type-case CVal (first args)
    [VNum (n) (if (< n (length (VTuple-l val)))
                  (m-return (list-ref (VTuple-l val) n))
                  (interp-error "not valid index for tuple"))]
    [else (interp-error "not valid index for tuple")]))

;remove item from hashmap
(define-primf (del dict key);VDict, CVal (key)
      (m-do ([contents (get-box (list (VDictM-b dict)))];contents is VDict
             [newDict (set-box (list (VDictM-b dict) (VDict (hash-remove (VDict-hashes contents) key))))])
             (VNone)))

; update hashmap
(define-primf (update d & args)
  (if (zero? (length args))
      (m-return (VNone))
      (type-case CVal (first args)
        [VDictM (b) (m-do ([contents (get-box (list b))]
             [toret (set-box (list (VDictM-b d) contents))])
             (VNone))]
        [else (interp-error "TypeError")]
        )))
           

;if item is in hashmap OR string is substring
(define-primf (in dict val);first val is attr, ret is a list of actual args
  (type-case CVal dict 
    [VDictM (box)
            (m-do ([contents (get-box (list box))])
                  (type-case (optionof CVal) (hash-ref (VDict-hashes contents) val)
                    [some (v) (VBool 1)]
                    [none () (VBool 0)]))]
    [VStr (s) (if (string-contains? (VStr-s val) s) (m-return (VBool 1)) (m-return (VBool 0)))]
    [VList (l) (if (member val l) (m-return (VBool 1)) (m-return (VBool 0)))]
    [else (interp-error "in needs to take iterable")]))

;tell if the item is iterable
(define (is-iterable it)
  (type-case CExp it
    [CTuple (l) true]
    [CList (l) true]
    [CStr (s) true]
    [else false]))

;returns a list of strings given a string
(define (get-str-list str)
  (map VStr (remove* (list "") (regexp-split "(0*)" str))))

(define (get-steps (l : (listof string)) (step : number) (lower : number) (upper : number) (rec : number) (total : string))   
  (cond
    [(empty? l)total]
    [(> rec upper) total]
    [(< rec lower) (get-steps (rest l) step lower upper (+ rec 1) total)]
    [(zero? (modulo (+ lower rec ) step))
     (get-steps (rest l) step lower upper (+ rec 1) (string-append total (first l)))]
    [else (get-steps (rest l) step lower upper (+ rec 1) total)])
  )

;list built-in func
(define-primf (list-f iter & ret)
  (type-case CVal iter
    [VTuple (l) (m-return (VList l))]
    [VList (l) (m-return (VList l))]
    [VStr (s) (m-return (VList (get-str-list s)))]
    [VNone () (m-return (VList empty))]
    [else (interp-error "argument not iterable")]))

;callable 
(define-primf (callable call & ret)
  (type-case CVal call
    [VClosure (a b c d) (m-return (VBool 1))]
    [VPrimF (id) (m-return (VBool 1))]
    [else (m-return (VBool 0))]))

;filter
(define-primf (Filter func & iter)
  (type-case CVal func
    [VNone () (type-case CVal (first iter)
                [VList (l) (m-return (VList (filter-none l)))]
                [VTuple (l) (m-return (VTuple (filter-none l)))]
                [else (interp-error "argument not iterable")])]
    [VClosure (a b c d) 
              (type-case CVal (first iter)
                [VList (l) (m-do ([filters (m-map (lambda (x) (apply-func func (list x) (VTuple empty))) l)]
                                  ;[error (interp-error "TypeError1")]
                                  )
                                        ;(if (= (length l) (length filters)) 
                                     (VList (filter-list filters l))
                                     ;(interp-error "TypeError"))
                                     )]
                [VTuple (l) (m-do ([filters (m-map (lambda (x) (apply-func func (list x) (VTuple empty))) l)]
                                  ;[error (interp-error "TypeError1")]
                                  )
                                        ;(if (= (length l) (length filters)) 
                                     (VTuple (filter-list filters l))
                                     ;(interp-error "TypeError"))
                                     )]
                [VStr (s) (m-do ([filters (m-map (lambda (x) (apply-func func (list x) (VTuple empty))) 
                                                    (map (lambda (x) (VStr x)) (string->list s 0)))]
                                  ;[error (interp-error "TypeError1")]
                                  )
                                        ;(if (= (length l) (length filters)) 
                                     (VStr (list->string (filter-list filters (string->list s 0))))
                                     ;(interp-error "TypeError"))
                                     )]
               #| [VList (l) (m-do ([filters (m-map (lambda (x) (apply-func func (list x) (VTuple empty))) l)])
                                 (if (= (length l) (length filters)) 
                                     (VList (filter-string filters l))
                                     (interp-error "TypeError")))]|#
                [else (interp-error "argument not iterable")])]
    [else (interp-error "nonvalid argument for filter")]))

(define (filter-none l)
  (foldr (lambda (x build)
           (type-case CVal x
             [VNone () build]
             [VList (elt) (if (empty? elt)
                              build
                              (cons x build))]
             [VStr (s) (if (= 0 (string-length s))
                              build
                              (cons x build))]
             [VNum (n) (if (= 0 n)
                              build
                              (cons x build))]
             [else (cons x build)])) empty l)
  )

(define (string->list s num)
  (if (= num (string-length s))
      empty
      (cons (substring s num (+ 1 num)) (string->list s (+ 1 num)))))

(define (list->string l)
  (foldr string-append "" l))
  

(define (filter-list filters original)
  (if (empty? original)
      empty
      (if (= (VBool-n (first filters)) 1)
          (cons (first original) (filter-list (rest filters) (rest original)))
          (filter-list (rest filters) (rest original)))))
          
(define-primf (isinstance test type)
  (type-case CVal type
    [VStr (s) 
          (type-case CVal test
            [VObj (dict class)
                  (m-do [(val (get-box (list class)))]
                        (if (string=? (VStr-s val) s)
                            (VBool 1)
                            (VBool 0)))]

            [VBool (v) 
                   (if (or (string=? "BOOL" s) (string=? "int" s)) (m-return (VBool 1)) (m-return (VBool 0)))]
            [VNum  (v) 
                   (if (string=? "int" s) (m-return (VBool 1)) (m-return (VBool 0)))]
            [else (interp-error "passing nontype")])]
    [else (interp-error "passing nonstring type")]))
    

                            

;tuple built-in func
(define-primf (tup iter & ret)
   (type-case CVal iter
    [VTuple (l) (m-return iter)]
    [VList (l) (m-return (VTuple l))]
    [VStr (s) (m-return (VTuple (get-str-list s)))]
    [VNone () (m-return (VTuple empty))]
    [else (interp-error "argument not iterable")]))

;;checks whether the 2 arguments are equal
(define-primf (equal left right)
  (type-case CVal left
    [VDictM (b) (if (VDictM? right)
               (m-do [(contents (get-box (list b)))
                      (contents2 (get-box (list (VDictM-b right))))]
                     (if (equal? contents
                                 contents2)
                         (VBool 1)
                         (VBool 0)))
               (m-return (VBool 0)))]
    [else (if (equal? left
              right)
      (m-return (VBool 1))
      (m-return (VBool 0)))]))


(define-primf (is left right)
  (type-case CVal left
    [VList (l)  (m-return 
                 (if (eqv? left right)
                     (VBool 1)
                     (VBool 0)))]
    [VTuple (t) (m-return 
                 (if (eqv? left right)
                     (VBool 1)
                     (VBool 0)))]
    [else (m-return (if (or (eqv? left right) (equal? left right))
                (VBool 1)
                (VBool 0)))]))

;;numeric addition
(define-primf (add left right)
  (let ((L (type-case 
              CVal left
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0]))
        (R (type-case 
              CVal right
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0])))   
  (if (not (or (equal? +nan.0 L) (equal? +nan.0 R)))
      (m-return (VNum
             (+ L R)))
      (interp-error "unhandled operator for +"))))

;;numeric subtraction
(define-primf (sub left right)
  (let ((L (type-case 
              CVal left
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0]))
        (R (type-case 
              CVal right
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0])))   
  (if (not (or (equal? +nan.0 L) (equal? +nan.0 R)))
      (m-return (VNum
             (- L R)))
      (interp-error "unhandled operator for -"))))

;;numeric negation
(define-primf (neg arg)
    (let ((L (type-case 
              CVal arg
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0])))
        
  (if (not (equal? +nan.0 L))
      (m-return (VNum (- 0 L)))
      (interp-error "unhandled operator for negate"))))

;;numeric plus-thing
(define-primf (pls arg)
    (let ((L (type-case 
              CVal arg
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0])))
        
  (if (not (equal? +nan.0 L))
      (m-return (VNum (+ 0 L)))
      (interp-error "unhandled operator for +"))))

;;invert, so instead of 2s complement we're gonna subtract
(define-primf (inv arg)
    (let ((L (type-case 
              CVal arg
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0])))
        
  (if (not (equal? +nan.0 L))
      (m-return (VNum (- 0 (+ 1 L))))
      (interp-error "unhandled operator for +"))))

;mod
(define-primf (mod left right)
    (if (zero? (VNum-n right))
         (interp-error "ZeroDivisionError")
         (m-return (VNum (modulo (VNum-n left) (VNum-n right))))))

;floor div
(define-primf (div-floor left right)
  (if (zero? (VNum-n right))
         (interp-error "ZeroDivisionError")
         (m-return (VNum (floor (/ (VNum-n left) (VNum-n right)))))))

;;numeric division(11/14)
(define-primf (div left right)
    (let ((L (type-case 
              CVal left
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0]))
        (R (type-case 
              CVal right
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0])))   
  (if (not (or (equal? +nan.0 L) (equal? +nan.0 R)))
  
      (if (zero? R)
         (interp-error "ZeroDivisionError")
         (m-return (VNum 
                    (/ L R))))
      (interp-error "unhandled operator for /"))))

;;numeric multiplication(11/14)
(define-primf (mult left right)
    (type-case CVal right
      [VStr (s) 
            (type-case CVal left 
              [VNum (n)   
                    (m-return
                     (VStr
                      (foldr (lambda (st base) (string-append (VStr-s st) base)) 
                             "" (list-mult-helper empty (get-str-list s) n))
                      ))]
              [else (interp-error "cannot multiply by string")])]
       [VList (l) 
            (type-case CVal left 
              [VNum (n)   
                    (m-return
                     (VList
                      (list-mult-helper empty l n)))]
              [else (interp-error "cannot multiply by string")])]
      [else 
       (let ((L (type-case 
                    CVal left
                  [VBool (n) n]
                  [VNum (n) n]
                  [VTuple (l) -nan.0]
                  [else +nan.0]))
             (R (type-case 
                    CVal right
                  [VBool (n) n]
                  [VNum (n) n]
                  [VTuple (l) -nan.0]
                  [else +nan.0])))
 
         (if (equal? -nan.0 R)
               (m-return
                (VTuple (tuple-mult-helper empty (VTuple-l right) (VNum-n left))))
               (if (not (or (equal? +nan.0 L) (equal? +nan.0 R)))
                   (m-return (VNum (* L R)))
                   (interp-error "unhandled operand for mult")))
         )]
      ))
      
     

(define (mult-helper current str n)
  (if (zero? n) current (mult-helper (string-append current str) str (- n 1)))
)

;string addition
(define-primf (str-add left right)
  (type-case
      CVal right
    [VStr (r)(m-return (VStr (string-append 
                      (VStr-s left)
                      r)))]
    [else (interp-error "operator not handled for string+other")]))
  
;string mult
(define-primf (str-mult left right)
  (m-return
   (VStr
    (foldr (lambda (st base) (string-append (VStr-s st) base)) 
       "" (list-mult-helper empty (get-str-list (VStr-s left)) (VNum-n right)))
     )))

;str-eq?
(define-primf (str-eq left right) 
               (type-case CVal right
                 [VStr (s) (m-return (if (string=? (VStr-s left) (VStr-s right)) (VBool 1) (VBool 0)))]
                 [VNone () (m-return (VBool 0))]
                 [else (interp-error "unsupported operand for str-compare")]))

;string gt
(define-primf (str-gt left right)
  (type-case CVal right
    [VStr (s) (m-return (if (string>? (VStr-s left) s) (VBool 1) (VBool 0)))]
    [else (interp-error "unsupported operand for str-gt")]))

;string gte
(define-primf (str-gte left right)
  (type-case CVal right
    [VStr (s) (m-return (if (string>=? (VStr-s left) s) (VBool 1) (VBool 0)))]
    [else (interp-error "unsupported operand for str-gt")]))

;str max
(define-primf (max-f val)
    (type-case CVal val
      [VStr (s) (m-return 
                 (VStr
                      (foldr (lambda (st base) (if (string>=? (VStr-s st) base) (VStr-s st) base)) 
                             "" (get-str-list s))
                      )) ]
      [else (interp-error "unsupported operand for min/max")]))

;str min
(define-primf (min-f val)
    (type-case CVal val
      [VStr (s) (m-return 
                 (VStr
                      (foldr (lambda (st base) (if (string<=? (VStr-s st) base) (VStr-s st) base)) 
                             "z" (remove* (list (VStr "")) (get-str-list s)))
                      )) ]
      [else (interp-error "unsupported operand for min/max")]))

;none eq?
(define-primf (none-eq left right)
  (m-return (type-case CVal right
                  [VNone () (VBool 1)]
                  [else (VBool 0)])))

;num gt
(define-primf (int-gt left right)
        (let ((L (type-case 
              CVal left
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0]))
        (R (type-case 
              CVal right
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0])))   
        (if (not (or (equal? +nan.0 L) (equal? +nan.0 R)))
            (m-return (if (> L R) (VBool 1) (VBool 0)))
            (interp-error "unsupported operator for num-compare"))))

;num gte
(define-primf (int-gte left right)
        (let ((L (type-case 
              CVal left
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0]))
        (R (type-case 
              CVal right
            [VBool (n) n]
            [VNum (n) n]
            [else +nan.0])))   
        (if (not (or (equal? +nan.0 L) (equal? +nan.0 R)))
            (m-return (if (>= L R) (VBool 1) (VBool 0)))
            (interp-error "unsupported operator for num-compare"))))
;num eq
(define-primf (int-eq left right)
          (let ((L (type-case 
                       CVal left
                     [VBool (n) n]
                     [VNum (n) n]
                     [VNone () -nan.0]
                     [else +nan.0]))
                (R (type-case 
                       CVal right
                     [VBool (n) n]
                     [VNum (n) n]
                     [VNone () -nan.0]
                     [else +nan.0])))
            
            (if (or (equal? -nan.0 L) (equal? -nan.0 R))
                (m-return (VBool 0))
                (if (not (or (equal? +nan.0 L) (equal? +nan.0 R)))
                      (m-return (if 
                               (= L R)
                               (VBool 1) (VBool 0)))
                    (interp-error "unsupported operator for num-compare")))))

;;gets a value from a box
(define-primf (get-box (box VBox?))
  (pm-lookup-store (VBox-v box)))

;;sets the value inside a box
(define-primf (set-box (box VBox?) val)
  (m-do ([(pm-add-store (VBox-v box) val)])
        val))

;;appends n tuples
(define-primf (tuple-append & (args (lambda (args) (andmap VTuple? args))))
  (m-return
   (VTuple
    (foldr (lambda (t l)
             (append (VTuple-l t)
                     l))
           empty
           args))))
;appends n lists
(define-primf (list-append & (args (lambda (args) (andmap VList? args))))
  (m-return
   (VList
    (foldr (lambda (t l)
             (append (VList-elts t)
                     l))
           empty
           args))))

;;multiplys a tuple by an int
(define-primf (tuple-mult (t VTuple?) (n VNum?))
  (m-return
   (VTuple
     (tuple-mult-helper empty (VTuple-l t) (VNum-n n)))))

(define (tuple-mult-helper start t n)
  (if (>= 0 n)
      start
      (tuple-mult-helper (append start t) t (- n 1))))
;;mulitiplies a list by an int
(define-primf (list-mult (t VList?) (n VNum?))
  (m-return
   (VList
     (list-mult-helper empty (VList-elts t) (VNum-n n)))))
     ;;(not strictly neccessary)

(define (list-mult-helper start t n)
  (if (>= 0 n)
      start
      (list-mult-helper (append start t) t (- n 1))))

(define-primf (gen-length t)
  (type-case CVal t
    [VTuple (l)  (m-return (VNum (length l)))]
    [VList (l) (m-return (VNum (length l)))]
    [VStr (s) (m-return (VNum (length (get-str-list s))))]
    [VDictM (b) (m-do ([contents (get-box (list b))])
                      (VNum (length (hash-keys (VDict-hashes contents)))))]
    [else (interp-error "undefined operand for len")]))

(define-primf (any (l VList?))
  (any-help l)
  )

(define (any-help l)
   (if (empty? (VList-elts l))
      (m-return (VBool 0))
      (type-case CVal (first (VList-elts l))
        [VNone () (any-help (VList (rest (VList-elts l))))]
        [VNum (n) (if (= n 0)
                      (any-help (VList (rest (VList-elts l))))
                      (m-return (VBool 1)))]
        [VBool (n) (if (= n 0)
                      (any-help (VList (rest (VList-elts l))))
                      (m-return (VBool 1)))]
        [else (interp-error "bad any argument in list")])))

(define-primf (all (l VList?))
  (all-help l)
  )

(define (all-help l)
   (if (empty? (VList-elts l))
      (m-return (VBool 1))
      (type-case CVal (first (VList-elts l))
        [VNone () (m-return (VBool 0))]
        [VNum (n) (if (= n 0)
                      (m-return (VBool 0))
                      (all-help (VList (rest (VList-elts l))))
                      )]
        [VBool (n) (if (= n 0)
                       (m-return (VBool 0))
                      (all-help (VList (rest (VList-elts l))))
                      )]
        [else (interp-error "bad all argument in list")])))

(define-primf (absv v)
  (type-case CVal v
    [VNum (n) (m-return (VNum (abs n)))]
    [VBool (n) (m-return (VNum (abs n)))]
    [else (interp-error "undefined operand for abs")]))

(define-primf (str v)
  (type-case CVal v
    [VBool (n) (if (= 1 n) (m-return (VStr "True")) (m-return (VStr "False")))]
    [VStr (n) (m-return v)]
    [else (interp-error "undefined operand for str")]))

;;bitwise list functions
(define-primf (bit-and (t VList?) (t2 VList?))
  (m-return
   (VList (foldr 
           (lambda (m l1) (remove m l1))     
               (append (VList-elts t) (VList-elts t2))
               (remove-duplicates (append (VList-elts t) (VList-elts t2)))
               ))))

(define-primf (bit-or (t VList?) (t2 VList?))
  (m-return
   (VList (remove-duplicates (append (VList-elts t) (VList-elts t2))))))

(define-primf (bit-xor (t VList?) (t2 VList?))
  (m-return
   (VList (remove* 
           (foldr 
            (lambda (m l1) (remove m l1)) 
               (append (VList-elts t) (VList-elts t2))
               (remove-duplicates (append (VList-elts t) (VList-elts t2))))
           (append (VList-elts t) (VList-elts t2))))))
(define-primf (bit-sub (t VList?) (t2 VList?))
  (m-return (VList (remove* (VList-elts t2) (VList-elts t)))))
;;finds the length of a tuple
(define-primf (tuple-length (t VTuple?))
  (m-return (VNum (length (VTuple-l t)))))
                   
;length of a list
(define-primf (list-length (t VList?))
  (m-return (VNum (length (VList-elts t)))))

;int cast
(define-primf (int val)
  (type-case CVal val
    [VNum (n) (m-return (VNum (floor n)))]
    [VStr (s) (m-return (VNum (floor (string->number s))))];need to deal with invalid string case
    [VBool (n) (m-return (VNum (floor n)))]
    [else (interp-error "undefined operand for int")]))

;float cast
(define-primf (float val)
  (type-case CVal val
    [VNum (n) (m-return (VNum (+ 0.0 n)))]
    [VStr (s) (m-return (VNum (+ 0.0 (string->number s))))];need to deal with invalid string case
    [VBool (n) (m-return (VNum (+ 0.0 n)))]
    [else (interp-error "undefined operand for int")]))


;get hash values
(define-primf (value (d VDictM?) & ret)
  (if (empty? ret)
      (m-do ([contents (get-box (list (VDictM-b d)))])
            (VList (hash-values (VDict-hashes contents))))
      (interp-error "TypeError")))

;get hash keys
(define-primf (keys (d VDictM?) & ret)
  (if (empty? ret)
      (m-do ([contents (get-box (list (VDictM-b d)))])
            (VList (hash-keys (VDict-hashes contents))))
      (interp-error "TypeError")))
  

(define-primf (clear (d VDictM?) & ret)
  (if (empty? ret)
      (set-box (list (VDictM-b d) (VDict (hash empty))))
      (interp-error "type Error")))

(define-primf (Range (n VNum?) & ret)
  (m-return (VList (map (lambda (x) (VNum x)) (range (VNum-n n)))))
  )

;constucting hash helper
(define (lists2ltup l1 l2)
  (if (empty? l1)
      (list)
      (cons (VTuple (list (first l1) (first l2))) (lists2ltup (rest l1) (rest l2)))))

;get hash items
(define-primf (items (d VDictM?) & ret)
    (if (empty? ret)
            (m-do ([contents (get-box (list (VDictM-b d)))])
                   (VList (lists2ltup (hash-keys (VDict-hashes contents)) (hash-values (VDict-hashes contents)))))
    (interp-error "TypeError")))

(define-primf (dict-length (d VDictM?) & rest)
  (m-do ([contents (get-box (list (VDictM-b d)))])
        (VNum (length (hash-keys (VDict-hashes contents))))))

(define-primf (construct (obj VObj?) & rest)
  (m-do ([dict (get-box (list (VObj-dict obj)))]
         [toret (type-case (optionof CVal) (hash-ref (VPrimMap-m dict) (VStr "__init__"))
                  [some (x) (m-return x)]
                  [none () (interp-error "Super class no bound")])])
        toret))

;;CURRENTXX
#|(define-primf (bool first & ret)
  (m-do ([tester (class-lookup (list first (VStr "__bool__")))])
        tester)
  )|#
 

  
;;finds the appropriate racket function for a given VPrimF symbol
(define (python-prim op) : ((listof CVal) -> (PM CVal))
  (case op
    [(print) print]
    [(list-f) list-f]
    [(equal) equal]
    [(float) float]
    [(int) int]
    ;[(bool) bool]
    [(int-add) add]
    [(int-sub) sub]
    [(int-neg) neg]
    [(int-pls) pls]
    [(int-inv) inv]
    [(int-mult) mult];(11/4)
    [(int-div) div];(11/4)
    [(div-floor) div-floor]
    [(mod) mod]
    [(int-gt) int-gt];(11/16)
    [(int-gte) int-gte];(11/16)
    [(int-eq) int-eq];(11/16)
    [(str-add) str-add];(11/4)
    [(str-mult) str-mult];(11/4)
    [(str-gt) str-gt];(11/16)
    [(str-gte) str-gte];(11/16)
    [(str-eq) str-eq];(11/16)
    [(none-eq) none-eq];(11/16)
    [(is) is]
    [(get-box) get-box]
    [(set-box) set-box]
    [(class-has-member?) class-has-member?]
    [(class-lookup) class-lookup]
    [(tuple-append) tuple-append]
    [(tuple-mult) tuple-mult]
    [(tuple-length) tuple-length]
    [(list-length) list-length]
    [(dict-length) dict-length]
    [(gen-length) gen-length]
    [(list-append) list-append]
    [(list-mult) list-mult]
    [(bit-and) bit-and]
    [(bit-sub) bit-sub]
    [(bit-or) bit-or]
    [(bit-xor) bit-xor]
    [(value) value]
    [(keys) keys]
    [(items) items]
    [(clear) clear]
    [(range) Range]
    ;[(asser-raises) asser-raises]
    [(in) in]
    [(get) get]
    [(update) update]
    [(del) del]
    [(absv) absv]
    [(str) str]
    [(tup) tup]
    [(min-f) min-f]
    [(max-f) max-f]
    [(get-tuple-val) get-tuple-val]
    [(any) any]
    [(all) all]
    [(callable) callable]
    [(filter) Filter]
    [(constructor) construct]
    [(isinstance) isinstance]
    ;[(BOOL) bool]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            interp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;returns the first n elements in lst
(define (take n lst)
  (cond
   [(or (empty? lst)
        (= n 0)) empty]
   [else (cons (first lst)
               (take (- n 1)
                     (rest lst)))]))

;;returns lst without the first n elements
(define (drop n lst)
  (cond [(empty? lst) empty]
        [(= n 0) lst]
        [else (drop (- n 1) (rest lst))]))

;constucting hash helper
(define (lists2hash l1 l2 h)
  (if (empty? l1)
      h
      (let ((h2 (hash-set h (first l1) (first l2))))
        (lists2hash (rest l1) (rest l2) h2))))


;;applies funct to args and varargs
(define (apply-func (func : CVal) (args : (listof CVal)) (varargs : CVal)) : (PM CVal) 
  (let ([args (append args
                      (VTuple-l varargs))])
    (type-case CVal func
      [VClosure
       (c-env off-args off-vararg body)
       ;(begin (display "function env: \n")
              ;(map (lambda (x) (display (string-append (symbol->string x) ", "))) (hash-keys c-env))
              ;(display "\n")
       (let ([named-args (take (length off-args) args)]
             [varargs (drop (length off-args) args)])
         (if (or (< (length args)
                    (length off-args))
                 (and (not (empty? varargs))
                      (none? off-vararg)))
             (interp-error
              (string-append "Application failed with arity mismatch\nfunction: "
                             (string-append (to-string func)
                                            (string-append "\nargs: "
                                                           (to-string args)))))
             (m-do ([new-env
                     (m-foldl
                      (lambda (pair env)
                        (local [(define-values (name val) pair)]
                               (m-do ([loc (add-new-loc val)])
                                     (hash-set env name loc))))
                      (type-case (optionof symbol) off-vararg
                        [none () (m-return c-env)]
                        [some (name)
                              (m-do ([loc (add-new-loc (VTuple varargs))])
                                    (hash-set c-env name loc))])
                      (map2 (lambda (x y)
                              (values x y))
                            off-args named-args))]
                    [(pm-catch-return (m-do ([(m-interp body
                                                        new-env)])
                                            (VNone))
                                      m-return)]))))]
      [VPrimF (id) ((python-prim id) args)]
      [else (interp-error (string-append "Applied a non-function: "
                                         (to-string func)))])))
(define (CMultSet-helper ids vals env)
  (if (= (length ids) (length vals))
      (if (= 1 (length ids))
                 (let ((id (first ids))(v (first vals)))
                   (type-case (optionof Location) (hash-ref env id)
                     [some (l) (pm-add-store l v)]
                     [none ()  (pm-catch-error (m-do ([g (get-global (symbol->string id))]
                                                      [(add-global (symbol->string id) v)]))
                                               (lambda (x)
                                                 (interp-error (string-append 
                                                                "undefined variable " 
                                                                (symbol->string id)))))]))
          (m-do
           ([(let ((id (first ids))(v (first vals)))
               (type-case (optionof Location) (hash-ref env id)
                 [some (l) (pm-add-store l v)]
                 [none ()  (pm-catch-error (m-do ([g (get-global (symbol->string id))]
                                                  [(add-global (symbol->string id) v)]))
                                           (lambda (x)
                                             (interp-error (string-append 
                                                            "undefined variable " 
                                                            (symbol->string id)))))]))]
            [(CMultSet-helper (rest ids) (rest vals) env)])))          
      (interp-error "Assignment does not have the same number of values as assignees")))

(define (m-interp expr env) : (PM CVal)
  (type-case CExp expr
    [CUndefined () (m-return (VUndefined))]
    [CNone () (m-return (VNone))]
    [CTrue () (m-return (VBool 1))]
    [CFalse () (m-return (VBool 0))]
    [CNum (n) (m-return (VNum n))]
    [CStr (s) (m-return (VStr s))]
    [CBox (v) (m-do ([val (m-interp v env)]; interps on v and adds to store
                     [loc (add-new-loc val)])
                    (VBox loc))]
    [CObj (d c)
          (m-do ([dict (m-interp d env)]
                 [class (m-interp c env)])
                (VObj dict class))]
    [CPrimMap (vals)
              (m-do ([contents
                      (m-map (lambda (pair)
                               (local [(define-values (key val) pair)]
                                      (m-do ([key (m-interp key env)]
                                             [val (m-interp val env)])
                                            (values key val))))
                             vals)])
                    (VPrimMap
                     (hash contents)))]
    [CTuple (l)
            (m-do ([contents (m-map (lambda (v)
                                      (m-interp v env))
                                    l)])
                  (VTuple contents))]

    [CId (x)
         (type-case (optionof Location) (hash-ref env x)
           [some (l)
                 (m-do ([store pm-get-store]
                        [(let ([v (type-case (optionof CVal) (hash-ref store l)
                                    [some (v) v]
                                    [none () (error 'interp
                                                    (string-append
                                                     "can't find loc for var: "
                                                     (to-string x)))])])
                           (if (VUndefined? v)
                               (interp-error (string-append "local used before it was defined: "
                                                            (to-string x)))
                                 (m-return v)
                           ))]))]
           [none () (get-global (symbol->string x))])]
    [CSet! (id v)
           (m-do ([v (m-interp v env)]
                  [(type-case (optionof Location) (hash-ref env id)
                                [some (l) (pm-add-store l v)]
                                [none ()  (pm-catch-error (m-do ([g (get-global (symbol->string id))]
                                                                 [(add-global (symbol->string id) v)]))
                                                          (lambda (x)
                                                            (interp-error (string-append 
                                                                           "undefined variable " 
                                                                           (symbol->string id))
                                                                         ; )))]))
                                                                   )))])]))]
    [CMultSet! (ids v)
           (m-do ([v (m-interp v env)]
                  [(CMultSet-helper ids (VTuple-l v) env)]))]
    [CLet (x bind body)
          (m-do ([val (m-interp bind env)]
                 [loc (add-new-loc val)]
                 [(m-interp body (hash-set env x loc))]))]
    [CAddGlobal (id bind)
                (m-do ([bind (m-interp bind env)]
                       [(add-global (symbol->string id) bind)])
                      bind)]
    [CSeq (e1 e2)
          (m-do ([(m-interp e1 env)]
                 [(m-interp e2 env)]))]
    [CFunc (args vararg body)
           (m-return (VClosure env args vararg body))]
    [CApp (func args varargs)
          (m-do ([func (m-interp func env)]
                 [args (m-map (lambda (arg) (m-interp arg env)) args)]
                 [varargs (m-interp varargs env)]
                 [(if (VObj? func)
                      (apply-func (VPrimF '__call__) (cons func args) varargs)
                      (apply-func func args varargs))]))]
    [CClassDef (name super body)
               (m-do ([new-bod (pm-catch-return (m-do ([(m-interp body
                                                        env)])
                                            (VNone))
                                      m-return)]
                      [dict (get-box (list (VDictM-b new-bod)))]
                      [sup (if (empty? super) 
                               (m-return (VNone))
                               (type-case (optionof Location) (hash-ref env (first super))
                                 [some (l)
                                       (m-do ([store pm-get-store]
                                              [(let ([v (type-case (optionof CVal) (hash-ref store l)
                                                          [some (v) v]
                                                          [none () (error 'interp
                                                                          (string-append
                                                                           "can't find loc for var: "
                                                                           (to-string (first super))))])])
                                                 (if (VUndefined? v)
                                                     (interp-error (string-append "local used before it was defined: "
                                                                                  (to-string (first super))))
                                                     (m-return v)
                                                     ))]))]
                                 [none () (get-global (symbol->string (first super)))]))]
                      [locforinit (add-new-loc (VPrimMap (VDict-hashes dict)))]
                      [locforclassname (add-new-loc (VStr (symbol->string (first super))))]
                      [loc1 (add-new-loc (VPrimMap 
                                          (if (empty? super)
                                              (hash-set (VDict-hashes dict) 
                                                        (VStr "__init__") 
                                                        (VObj (VBox locforinit) (VBox locforclassname)))
                                              (hash-set 
                                               (hash-set (VDict-hashes dict) 
                                                         (VStr "__init__") 
                                                         (VObj (VBox locforinit) (VBox locforclassname)))
                                               (VStr "__super__") 
                                               sup))))]
                      [loc2 (add-new-loc (VStr "type"))]
                      [obj (m-return (VObj (VBox loc1) (VBox loc2)))]
                      [(add-global (symbol->string name) obj)])
                      obj)] 
    [CCmp (iter func)
          (m-do ([func (m-interp func env)]
                 [arg (m-interp iter env)]
                 [test (m-map (lambda (x) (apply-func func (list x) (VTuple empty))) (VList-elts arg))])
                 (VList test))
                ]
    [CReturn (v)
             (m-do ([v (m-interp v env)]
                    [(pm-return v)]))]
    [CPrimF (id) (m-return (VPrimF id))]
    [CIf (test t e)
         (m-do ([test-v (m-interp test env)]
                [(if (equal? test-v (VBool 1))
                     (m-interp t env)
                     (m-interp e env))]))]
    [CRaise (type msg)
              (if (string=? type "ReRaise")
                (m-interp last_raise env)
                (begin 
                  (set! last_raise (CRaise type msg))
                (let ((pret-args 
                       (foldl 
                        (lambda (a b) (string-append b a))
                        (string-append type " : ") (map (lambda (s) (string-append s ", ")) msg))))
                  (m-interp (CError (CStr pret-args)) env))))
              ]
    [CTryExcp (try name except e as)
                (pm-try-catch (m-interp try env) 
                              (lambda (error) 
                                   (if (or (string=? (first (string-split (VStr-s error))) name) (string=? name "ExceptAll")) ; if the exception is to be caught
                                         (if (string=? "" (symbol->string as)) ;;if there is no "as e"
                                             (m-interp except env)
                                             (m-interp (CLet as (CStr (VStr-s error)) except) env))
                                         (m-interp (CRaise (VStr-s error) (list)) env)))
                              (lambda (x) 
                               (m-interp e env))
                              )]
    [CTryFinal (try final)
              (pm-try-catch (m-interp try env) 
                              (lambda (error) ; this will  be called if an error has been thrown and not caught
                                    (m-interp (CSeq final (CRaise (VStr-s error) (list))) env)) ;;(interp-error (VStr-s error)))))
                              (lambda (x) (m-interp final env))
                              )]
    [CError (val)
            (m-bind (m-interp val env) pm-error)]

    [CList (elts) (m-do ([contents (m-map (lambda (v)
                                      (m-interp v env))
                                    elts)])
                  (VList contents))]
    [CDict (keys values) 
           (m-do ([k (m-map (lambda (v)
                                      (m-interp v env))
                                    keys)] ;;m-do interprets shit and deals with the store, then stores the result in variables k and v
                  [v (m-map (lambda (v)
                                      (m-interp v env))
                                    values)])
           (VDict (lists2hash k v (hash empty))))]
    [CDictM (b) (m-do
                 ([contents (m-interp b env)])
            (VDictM contents))]
    [CLocals (l) 
                   (m-do 
                    ([vals (m-map (lambda (x) 
                                    (m-interp (CId x) env))
                                    l)]
                     [loc (add-new-loc (VDict 
                                        (lists2hash (map (lambda (x) (VStr (symbol->string x))) l) vals (hash empty))))])
                    (VDictM (VBox loc)))]
    [CDictLoad (dict key) 
               (m-do ([d (m-interp dict env)]
                      [contents (get-box (list (VDictM-b d)))]
                      [k (m-interp key env)])
                     (type-case (optionof CVal) (hash-ref (VDict-hashes contents) k)
                       [some (v) v]
                       [none () (VNone)]))]
    [CDictStore (dict key) (m-interp dict env)];;temp
    [CAssign (to from) 
             (m-do ([d (type-case CExp to
                         [CDictStore (dict key) (m-interp dict env)]
                         [else (error 'interp "Invalid assignment type")])]
                    [k (type-case CExp to
                         [CDictStore (dict key) (m-interp key env)]
                         [else (error 'interp "Invalid assignment type. Also, how the fuck did you get here?")])]
                    [contents (get-box (list (VDictM-b d)))]
                    [v (m-interp from env)]
                    [newDict (set-box (list (VDictM-b d) (VDict (hash-set (VDict-hashes contents) k v))))])
                   (VNone))]
;rhis is done in a really silly way.
    [CSlice (val lower upper step)
            (m-do ([l (m-interp val env)]
                   [up (m-interp upper env)]
                   [low (m-interp lower env)]
                   [stp (m-interp step env)])
                  (type-case CVal l
                    [VStr (s)
                          (if (< (VNum-n stp) 0)
                              (VStr (get-steps (reverse (remove* (list "") (regexp-split "(~*)" s))) 
                                               (abs (VNum-n stp)) 
                                               (if (or (<= (VNum-n low) 0) (>= (VNum-n low) (string-length s)))
                                                   0
                                                   (max 0 (abs (- (VNum-n low) (- (string-length s) 1)))))
                                               (min (string-length s) (abs (- (VNum-n up)  (- (string-length s) 1)))) 
                                               0 
                                               "" ))
                              (VStr (get-steps (remove* (list "") (regexp-split "(~*)" s))
                                               (VNum-n stp) (max 0 (VNum-n low)) (min (VNum-n up) (string-length s)) 0 "" )))]
                    [else (error 'interp "nonstring type given to slice - not implemented")])
            )]
                   
    ))

(define (interp expr)
  (local [(define-values (store res)
            ((m-interp expr (hash (list)))
             empty-store))]
         (type-case (ROption CVal) res
           [RValue (v) v]
           [RReturn (v) (error 'interp "returned when not in function context")]
           [RError (v) (error 'interp (to-string v))])))


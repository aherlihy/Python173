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
(require (typed-in racket [string->number : (string -> number)]))
(require (typed-in racket [regexp-split : (string string -> (listof string))]))
(require (typed-in racket [remove* : ((listof 'a) (listof 'a) -> (listof 'a))]))
(require (typed-in racket [remove : ('a (listof 'a) -> (listof 'a))]))
(require (typed-in racket [hash-values : ((hashof 'a 'b) -> (listof 'b))]))
(require (typed-in racket [hash-keys : ((hashof 'a 'b) -> (listof 'a))]))
(require (typed-in racket [string-split : (string -> (listof string))]))
(require (typed-in racket [number->string : (number -> string)]))
(require (typed-in racket [hash->list : ((hashof 'a 'a) -> (listof (listof 'a)))]))
(require (typed-in racket [remove-duplicates : ((listof 'a) -> (listof 'a))]))
(require (typed-in racket [abs : (number -> number)]))

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


(define last_raise (CRaise "RuntimeError" (list))) ; keep track of prev raise for re-raises

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
           (interp-error "primf not given enough arguments")
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
                                         (to-string key)))])];why would be passed __add__
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
    (m-do ([contents (get-box (list (VDictM-b val)))])
          (type-case (optionof CVal) (hash-ref (VDict-hashes contents) (first ret))
            [some (v) v]
            [none () (cond
                       [(equal? 1 (length ret)) (VNone)]
                       [(equal? 2 (length ret)) (second ret)]
                       [else (error 'interp "TypeError")])])))

;remove item from hashmap
(define-primf (del dict key);VDict, CVal (key)
      (m-do ([contents (get-box (list (VDictM-b dict)))];contents is VDict
             [newDict (set-box (list (VDictM-b dict) (VDict (hash-remove (VDict-hashes contents) key))))])
             (VNone)))

; update hashmap
(define-primf (update d & args)
      (m-do ([contents (get-box (list (VDictM-b (first args))))]
             [toret (set-box (list (VDictM-b d) contents))])
             (VNone)))
           

;if item is in hashmap OR string is substring
(define-primf (in dict val);first val is attr, ret is a list of actual args
  (type-case CVal dict 
    [VDictM (box)
            (m-do ([contents (get-box (list box))])
                  (type-case (optionof CVal) (hash-ref (VDict-hashes contents) val)
                    [some (v) (VBool 1)]
                    [none () (VBool 0)]))]
    [VStr (s) (interp-error "need to handle string in")]
    [VList (l) (if (member val l) (m-return (VBool 1)) (m-return (VBool 0)))]
    [else (interp-error "in need to take iterable")]))

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

;list built-in func
(define-primf (list-f iter)
   (type-case CVal iter
    [VTuple (l) (m-return (VList l))]
    [VList (l) (m-return (VList l))]
    [VStr (s) (m-return (VList (get-str-list s)))]
    [VNum (n) (m-return (VList empty))]
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
  (m-return (if (or (eqv? left right) (equal? left right))
                (VBool 1)
                (VBool 0))))

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
         (interp-error "divide by 0 error")
         (m-return (VNum 
                    (/ L R))))
      (interp-error "unhandled operator for /"))))

;;numeric multiplication(11/14)
(define-primf (mult left right)
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
             (VTuple
              (tuple-mult-helper empty (VTuple-l right) (VNum-n left))));tempory solution, ask William
        (if (not (or (equal? +nan.0 L) (equal? +nan.0 R)))
            (m-return (VNum
             (* L R)))
            (type-case CVal left
              [VStr (s) (if (equal? +nan.0 R)
                            (interp-error "unhandled operator for string*")
                            (m-return (VStr (mult-helper "" s R))))]
              [else (type-case CVal right
                      [VStr (s) (if (equal? +nan.0 L)
                            (interp-error "unhandled operator for *string")
                            (m-return (VStr (mult-helper "" s L))))]
                      [else (interp-error "unhandled operator for string*")])])))))
      
     

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
  (m-return right))

;str-eq?
(define-primf (str-eq left right) 
               (type-case CVal right
                 [VStr (s) (m-return (if (string=? (VStr-s left) (VStr-s right)) (VBool 1) (VBool 0)))]
                 [VNone () (m-return (VBool 0))]
                 [else (interp-error "unsupported operator for str-compare")]))

;string gt
(define-primf (str-gt left right)
  (type-case CVal right
    [VStr (s) (m-return (if (string>? (VStr-s left) s) (VBool 1) (VBool 0)))]
    [else (interp-error "unsupported operator for str-gt")]))

;string gte
(define-primf (str-gte left right)
  (type-case CVal right
    [VStr (s) (m-return (if (string>=? (VStr-s left) s) (VBool 1) (VBool 0)))]
    [else (interp-error "unsupported operator for str-gt")]))

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
                    (m-return (if (= L R) (VBool 1) (VBool 0)))
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
    [else (interp-error "undefined operand for len")]))

(define-primf (absv v)
  (type-case CVal v
    [VNum (n) (m-return (VNum (abs n)))]
    [VBool (n) (m-return (VNum (abs n)))]
    [else (interp-error "undefined operand for abs")]))

(define-primf (str v)
  (type-case CVal v
    [VBool (n) (if (= 1 n) (m-return (VStr "True")) (m-return (VStr "False")))]
    [else (interp-error "undefined operand for str")]))
;;bitwise list functions
(define-primf (bit-and (t VList?) (t2 VList?))
  (m-return
   ;(VList (remove* (remove-duplicates (append (VList-elts t) (VList-elts t2))) (append (VList-elts t) (VList-elts t2))))))
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

;bool cast
(define-primf (bool val)
  (m-return (VBool 1)))

;get hash values
(define-primf (value (d VDictM?))
  (m-do ([contents (get-box (list (VDictM-b d)))])
        (VList (hash-values (VDict-hashes contents)))))
;get hash keys
(define-primf (keys (d VDictM?))
  (m-do ([contents (get-box (list (VDictM-b d)))])
        (VList (hash-keys (VDict-hashes contents)))))

(define-primf (clear (d VDictM?))
  (set-box (list (VDictM-b d) (VDict (hash empty)))))

;constucting hash helper
(define (lists2ltup l1 l2)
  (if (empty? l1)
      (list)
      (cons (VTuple (list (first l1) (first l2))) (lists2ltup (rest l1) (rest l2)))))

;get hash items
(define-primf (items (d VDictM?))
  (m-do ([contents (get-box (list (VDictM-b d)))])
         (VList (lists2ltup (hash-keys (VDict-hashes contents)) (hash-values (VDict-hashes contents))))))

(define-primf (asst-raises val & ret)
  (begin (display "asst-raises interp") (m-return (VBool 1))))
  
;;finds the appropriate racket function for a given VPrimF symbol
(define (python-prim op) : ((listof CVal) -> (PM CVal))
  (case op
    [(print) print]
    [(list-f) list-f]
    [(equal) equal]
    [(float) float]
    [(int) int]
    [(bool) bool]
    [(int-add) add]
    [(int-sub) sub]
    [(int-neg) neg]
    [(int-pls) pls]
    [(int-inv) inv]
    [(int-mult) mult];(11/4)
    [(int-div) div];(11/4)
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
    [(gen-length) gen-length]
    [(list-append) list-append]
    [(list-mult) list-mult]
    [(bit-and) bit-and]
    [(bit-or) bit-or]
    [(bit-xor) bit-xor]
    [(value) value]
    [(keys) keys]
    [(items) items]
    [(clear) clear]
    ;[(asser-raises) asser-raises]
    [(in) in]
    [(get) get]
    [(update) update]
    [(del) del]
    [(asst-raises) asst-raises]
    [(absv) absv]
    [(str) str]
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
                               (m-return v)))]))]
           [none () (interp-error (string-append "Unbound identifier: "
                                                 (to-string x)))])]
    [CSet! (id v)
           (m-do ([v (m-interp v env)]
                  [(type-case (optionof Location) (hash-ref env id)
                     [some (l) (pm-add-store l v)]
                     [none () (error 'interp (string-append "variable never bound:"
                                                            (to-string id)))])]))]
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
                 [(apply-func func args varargs)]))]
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
    [CTryExcp (try name except e)
              (pm-try-catch (m-interp try env) 
                              (lambda (error) 
                                 (if (or (string=? (first (string-split (VStr-s error))) name) (string=? name "ExceptAll")) 
                                  (m-interp except env)
                                    (m-interp (CRaise (VStr-s error) (list)) env))) ;;(interp-error (VStr-s error)))))
                              (lambda (x) (m-interp e env))
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
                   
    ))

(define (interp expr)
  (local [(define-values (store res)
            ((m-interp expr (hash (list)))
             empty-store))]
         (type-case (ROption CVal) res
           [RValue (v) v]
           [RReturn (v) (error 'interp "returned when not in function context")]
           [RError (v) (error 'interp (to-string v))])))


                   .-.
 aecarlso         /  aa
 aherlihy         \ -,_)
             _..._| \  `-<
           ." .__.' |
          (        /`\
         (`'------'  /
         /;._______.'\
         \           /
          '-.......-'

           173 PYTHON

Please note that we have been given an extension until the end of exam period. This is by no means our final handin.
Tests Passing
- dict
- lists
- tuples
- bool (except 1)
- exceptions (except 2)
- types (except 1)
- 5 in scope

Tests Not Passing
- builtin (not implemented)
- scope (partially implemented)
- smattering listed above

MONAD

We worked off of design 3. The most confusing portion of our code is python-monad.rkt. Our syntax builders are stored in monad and return values of type PM (aka, return values with a store. PM is a wrapper for ROptions (RValue, RReturn, RError), which contain CVals, and the store. To summarize, PM contains a store and ROption, and ROption contains a CVal.) Most of the time, we are calling m-do or m-return on CVals and the store in interp, wich returns PMs, our final return values. Additionally, we call different monadic functions for error catching or returning, such as pm-catch-error and pm-catch-return. Monad also includes methods to modify the store directly, such as pm-get-store and pm-set-store. 

LIB
Aside from Monad, the only signifigant addition to the stencil code is CPrimF type. All built-in function calls are stored in lib-binds which binds the name of the called function to the methods in interp that execute them (often inside of CPrimFs). These functions are defined in interp using CPrimF, whcih recognises them as primative functions and calls them on the appropriate arguments uing racket functions.

We define our basic types in python-lib using the 'make-type' syntax builder (also defined in lib). We are able to call the same functions on different types and have them execute different primfs because within make-type we map the function calls to the specific primfs we want executed for each type. In terms of built-in functions that are called on instances of a give type, we pass the value to the function as its implicit first argument (i.e. for dict clear). 

Asserts are also defined in lib. Other than assert-raises, we create a CFunc with the corresponding function (i.e 'is' for AssertIs,'in' for AssertIn, etc.) as an CApp within the function. The extra CTuple in CApp and CFunc deal with the varargs (for clarification, see apply-func). 

DESUGAR
Relatively straight forward, we keep most of the functionality to converting between Py-types and C-types. The bulk of the work is passed to lib or interp. The PyOp type contains mappings from the unary and binary op types that are passed from the JSON to the functions that execute them.

INTERP
All of our function definitions are stored here for built-in types and various other helpers. The environment is passed around, and we are able to access and alter values in the store using functions such as get-globals and set-globals. Within interp we map the VPrimFs from lib to actual functions within python-prim. Interp itself is relatively straight forward, we just convert CExpressions from desugar to CValues that are returned using monadic functions that wrap them in ROptions. We are able to implement exceptions because we can examine this return type to see if it is an RError or RValue, using pm-try-catch. We were able to create mutable dictionaries using boxes, which are their own type. CBoxes contain a list of CValues, while a VBox contains the location at which these values are stored. We wrapped our boxes in VDictMs, which contain the location only. VDicts contained the hashmap upon which we built our dictionaries. 

The other files are pretty much self-explanatory. We defined CValues and CExpressions within python-core-syntax.rkt, and PyExpressions within python-syntax.rkt.

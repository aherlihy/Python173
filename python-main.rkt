#lang racket/base

(require racket/cmdline
         racket/pretty
         "parse-python.rkt"
         "get-structured-python.rkt"
         "python-interp.rkt"
         "python-desugar.rkt"
         "python-lib.rkt"
         "run-tests.rkt"
         "python-evaluator.rkt")

(define (python-test-runner _ port)
  (run-python port))

(define (run-python port)
  (interp
    (python-lib;kind of like library functions that are defined in the env from the start. 
               ;not syntax but not worth desugaring.
      (desugar;takes in PyExpr, i.e. surface syntax and produces core python
  ;;(display
        (get-structured-python;takes in JSON, produces a PyExpr (represents user input)
  ;;(display
          (parse-python/port port python-path))))));takes in cmdline input and returns JSON structure

(define python-path "/home/joe/bin/python")

(command-line
  #:once-each
  ("--interp" "Interpret stdin as python"
   (run-python (current-input-port)))

  ("--interp-py" "Interpret stdin as python using py-prelude.py"
   (define results ((mk-python-cmdline-eval python-path) "stdin" (current-input-port)))
   (display (car results) (current-output-port))
   (display (cdr results) (current-error-port)))

  ("--get-syntax" "Get s-exp for python"
   (pretty-write (parse-python/port (current-input-port) python-path)))

  ("--test" dirname "Run all tests in dirname"
   (display (results-summary (run-tests (mk-proc-eval/silent python-test-runner) dirname))))

  ("--test-py" dirname "Run all tests in dirname using python"
   (display (results-summary (run-tests (mk-python-cmdline-eval python-path) dirname))))

  ("--python-path" path "Set the python path"
   (set! python-path path))

)

;; metacircular interpreter for a subset of scheme
;; Starter code author: Alistair Campbell

;; Completed by: Madison Monroe

;; Hamilton College

;; CS 220 - Principles of Programming Languages
;; Spring 2022


;;popl-make-empty-environment
;; Return a list consisting of an empty association list
(define (popl-make-empty-environment)
   '(()))

;; popl-copy-env
;; Make a shallow copy of an environment by creating a new singleton list having the same association list as the given environment.
;; env
;; (|)
;;  |
;;  v
;;  ((sym1 val1) (sym2 val2) ... )
;;  ^
;;  |
;; (|)
;; return-value
(define (popl-copy-environment env)
   (list (car env)))


;; popl-bind
;; Given a symbol, a value, and an environment
;; 1) cons the symbol/value pair to the front of the association list
;; 2) Change the car of the environment to the new association list
;; 3) return the symbol
;; Example:
;; => (define env1 (popl-make-empty-environment))
;; (())
;; => (popl-bind 'b 2 env1)
;; b
;; => env1
;; (((b 2)))
(define (popl-bind symbol value env)
  (let* ((temp (cons (list symbol value) (car env))))
        (set-car! env temp))
    symbol)

;; popl-get-binding
;; Given a symbol and an environment
;; return the symbol/value pair from the
;; environment having that symbol
;; Example, if env1 is ((b 1) (y 6) (b 3))
;; then (popl-get-binding y env1) is (y 6)
;; Return #f if no such binding exists
;; You can use the built-in assoc function for
;; this.

(define (popl-get-binding symbol env)
    (let ((pair (assoc symbol (car env))))
      (if (not pair) #f
          pair)))

;; popl-env-value
;; Given a symbol and an environment return the value associated with
;; that symbol.  Use the error function if there is no value for the symbol
(define (popl-env-value symbol env)
    (let ((pair (assoc symbol (car env))))
      (if (not pair) (error "No value found")
          (cadr pair))))

;; popl-set!
;; Given a symbol, an environment, and a new value, 1) get the existing
;; binding pair (error if none), 2) get the old value, 3) change the value
;; of the pair to the new value, and 4) return the old value
(define (popl-set! symbol newval env)
    (let* ((binding (popl-get-binding symbol env))
           (old-value (popl-env-value symbol env))
           )
        (set-car! (cdr binding) newval)
        old-value))


(define *TOPENV* (popl-make-empty-environment)) ;; the top-level environment
(define *LOOPTOP* #!unspecific)        ;; continuation at the top of the REPL
(define *LAMBDA* (list 'popl-lambda))  ;; special value unique to lambdas

;; When errors occur, we handle them by displaying things
;; and jumping back to the top of the REPL.
(define (popl-error . things)
  (display "ERROR: ")
  (for-each display things)
  (newline)
  (*LOOPTOP* 'dontcare))   ;; go back to the top of the REPL

;; define all the primitive procedures here:
(popl-bind '+ + *TOPENV*)
(popl-bind '- - *TOPENV*)
(popl-bind '* * *TOPENV*)
(popl-bind '/ / *TOPENV*)
(popl-bind '= = *TOPENV*)
(popl-bind 'cons cons *TOPENV*)
(popl-bind 'car car *TOPENV*)
(popl-bind 'cdr cdr *TOPENV*)
(popl-bind 'null? null? *TOPENV*)
(popl-bind 'eq? eq? *TOPENV*)
(popl-bind 'equal? equal? *TOPENV*)
(popl-bind 'list list *TOPENV*)


;; Yes, you can use scheme's if to implement this.
;; It's not cheating!

;(if cond then else)
; (if cond
;    a
;    b)
(define (popl-eval-if expr env)
        (if (popl-eval (second expr) env)
            (popl-eval (third expr) env)
            (popl-eval (fourth expr) env)))

;expr is in the form
;((cond1
;       then1)

;      (cond2
;       then2)

;      (cond3
;       then3)

;      (else
;       action4))
(define (popl-eval-cond expr env)
    (if (or (null? expr) (null? caar exp) (null? cdar expr))
            (popl-error "Ill formed cond statment.")

        (if (eq? (caar expr) 'else)
            (popl-eval (cdar expr) env)

            (if (popl-eval (caar expr) env)
                (popl-eval (second (car expr)) env)
                (popl-evaluate-cond (cdr expr) env)
            )
        )
    )
)

(define (popl-eval-set! expr env)
'add later
)

(define (popl-eval-let* expr env)
'add later
)

(define (popl-eval-let expr env)
'add later
)

;; given a non-primitive function,
;; make a copy of the function's environment
;; and with that copy,
;; 1. bind each parameter to corresponding
;;    arguments,
;; 2. evaluate each
;; element of the function's body.
;; 3. Return the last value.

(define (popl-apply function arguments)
;fourth element of lambda is the environment
;copy env
'dolater
 )

;; Evaluate all the elements of expr,
;; which is a list.
;; If the first element of the list is a procedure
;;   (primitive function), then use scheme's apply
;;   function.
;; Else if the first element is a non-primitive function
;;   (begins with *LAMBDA*), then use popl-apply.
;; Otherwise use popl-error.
(define (popl-eval-function-call expr env)
   (let* ((all-evaluated (map (lambda (e) (popl-eval e env)) expr))
          (fun (car all-evaluated))
          (args (cdr all-evaluated)))
      (apply fun args)))

;add syntax checking and errors
;ex if we write (lambda) is bad or (lambda ())
(define (popl-eval-lambda expr env)
   ;; expr is something like:  (lambda (a b c) form1 form2)
   ;; Error checking needs to be added to this function ensuring
   ;; that the parameter list is a list containing only
   ;; symbols with no duplicates.
   (list *LAMBDA*             ;; special object indicating it's a lambda
         (second expr)        ;; parameter list
         (cddr expr)          ;; function body (list of expressions)
         env))                ;; the current environment

;; The name of the main evaluator is popl-eval-help
;; so that we can do enhanced tracing. See further comments below.
(define (popl-eval-help expr env)
  (cond ((or (number? expr)
             (boolean? expr)
             (string? expr)
             (null? expr)) expr)
        ((symbol? expr) (popl-env-value expr env))
        ((pair? expr)
         (cond ((eq? (first expr) 'define)
                (let ((sym (second expr))
                      (val (popl-eval (third expr) env)))
                  (popl-bind sym val env)))
               ((eq? (first expr) 'quote)
                (second expr))
               ((eq? (first expr) 'lambda)
                (popl-eval-lambda expr env))
               ((eq? (first expr) 'if)
                (popl-eval-if expr env))
               ((eq? (first expr) 'cond)
                (popl-eval-cond (cdr expr) env))
               ((eq? (first expr) 'set!)
                (popl-eval-set! expr env))
                (popl-eval-equal? expr env))
               ((eq? (first expr) 'let)
                (popl-eval-let expr env))
               ((eq? (first expr) 'let*)
                (popl-eval-let* expr env))
               (else (popl-eval-function-call expr env))))
        (else (popl-error "(internal) unknown object " expr " passed to evaluator"))))


;; Change this to #f before submitting:
(define *ENABLE-DEBUG* #t)

;; Execute a thunk n times.
(define (dotimes n something)
   (cond ((= n 0))
         (else (something) (dotimes (- n 1) something))))

;; A function for writing things for debugging.
;; Takes an indententation level, a string prefix,
;; and other things.
(define (popl-debug-println level prefix . others)
  (if *ENABLE-DEBUG* (begin
    ; indent
    (dotimes level (lambda () (display " ")))
    ; write the things
    (display prefix)
    (for-each write others)
    ; newline terminate
    (newline))))


;; This popl-eval is an augmentation that displays what's being
;; evaluated and what's being returned. Hopefully it's helpful
;; for debugging.
(define popl-eval
  (let ((eval-level 0))   ;; keeps track of current recursion depth
    (lambda (expr env)
      (set! eval-level (+ eval-level 1))
      (popl-debug-println eval-level "Evaluating: " expr)
      (let ((result (popl-eval-help expr env)))
        (popl-debug-println eval-level "Returning: " result)
        (set! eval-level (- eval-level 1))
        result))))


;; This popl-repl is implemented with a continuation loop
(define (popl-repl)
   (call-with-current-continuation (lambda (c) (set! *LOOPTOP* c)))
   (display "H]=> ")                                    ;prompt
   (let ((expr (read)))                                 ;Read
      (cond ((equal? expr '(exit)) "Goodbye")
            (else (write (popl-eval expr *TOPENV*))     ;Eval, Print
                  (newline)
                  (*LOOPTOP* 'dontcare)))))             ;Loop


;; end

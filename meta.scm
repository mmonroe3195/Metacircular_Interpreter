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
           (old-value (popl-env-value symbol env)))
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
(popl-bind '* * *TOPENV*)
(popl-bind 'cons cons *TOPENV*)
(popl-bind 'car car *TOPENV*)
(popl-bind 'cdr cdr *TOPENV*)
(popl-bind 'null? null? *TOPENV*)
(popl-bind 'eq? eq? *TOPENV*)
(popl-bind 'equal? equal? *TOPENV*)
(popl-bind 'list list *TOPENV*)

(define (popl-eval-if expr env)
    (let ((expr-len (list-length expr)))
        ;checking that the if statement is formed properly
        (if (or (> expr-len 4) (< expr-len 3))
            (popl-error "Ill-formed syntax: " expr)))

    ;checking if the condition clause is true
    (if (popl-eval (second expr) env)
        ;if the condition clause is true, then execute the then clause
        (popl-eval (third expr) env)
        ;if the condition clause is false, then execute the else clause
        (popl-eval (fourth expr) env)))

(define (popl-eval-cond expr env)
    (let ((body (cdr expr)))
        ;error checking to ensure cond is in the correct form
        (for-each (lambda (e)
            (if (or (not (pair? e)) (and (pair? (car e)) (null? (cdr e))))
                (popl-error "Ill formed syntax: " expr))) body)

        ;if the expression is "(cond)", the helper function is not called
        (if (not (null? body))
            (popl-eval-cond-helper body env))))

(define (popl-eval-cond-helper expr env)
        (if (not (null? expr))
            (let ((condition (caar expr))
                  (then-block (cadar expr))
                  (rest (cdr expr)))

            ;if it is an else
            (if (eq? 'else condition)
                ;checking if else is last condition
                (if (null? rest)
                    ;if it is the last condition, evaluate the then body
                    (popl-eval then-block env)
                    ;if there are more conditions after else, error
                    (popl-error "Ill formed cond statement."))

                ;using a cond and recersively evaluating every condition with helper fn
                (cond ((popl-eval condition env) (popl-eval then-block env))
                ;if the first if-block is not true, and there is another condition to
                ; evaluate, then call the helper function on the rest
                ((not (null? rest)) (popl-eval-cond-helper rest env)))))))

;determines the new value for a binding
(define (set!-helper new-val env)
    ;if the new-val has a binding for it, we call set!-helper again on that symbol it is bound to
    ;if the new-val does not have a binding, we return the new-val
    (if (eq? #f (popl-get-binding new-val env))
        new-val
        (set!-helper (popl-env-value new-val env) env)))

(define (popl-eval-set! expr env)
    (let ((expr-len (list-length expr)))

        ;checking for errors in set! formation
        (if (= expr-len 1)
            (popl-error "Ill-formed syntax: " expr))
        (let ((symbol (cadr expr)))
             (cond
                   ;if the expression is of the wrong size or the symbol is not a correct symbol
                   ((or (> expr-len 3) (not (symbol? symbol)))
                    ((popl-error "Variable required in this context: " symbol)))
                   ;if there isn't a current binding, signal an error.
                   ((eq? #f (popl-get-binding symbol env))
                    (popl-error "Unbound variable: " symbol))
                   ;if the expression is of the correct length
                   ((= expr-len 3)
                    (let* ((old-val (popl-env-value symbol env))
                    (symbol-val (popl-eval (caddr expr) env))
                    (new-value (set!-helper symbol-val env)))

                    ;updates the current value of the variable
                    (popl-set! symbol new-value env)

                    ;returns the previous value of the variable
                    old-val))))))

;given an expression, checks if let or let* is in the proper form
(define (proper-let expr)

    ;checking if the expression is of the proper length
    (if (< (list-length expr) 3)
        (popl-error "Ill formed syntax: " expr))

        (let ((binding-lst (second expr))
              (body (cddr expr)))

             ;checking to see if there are too many nested lists or not enough parenthesis in the binding list
             (if (or (not (list? (car binding-lst))) (list? (caar binding-lst)))
                (popl-error "Ill formed syntax: " expr))

             ;checking to see if each element in the binding list is a pair and that there aren't too many nested lists
             ;if not, signal an error.
             (for-each (lambda (e)
                 (if (or (not (pair? e)) (list? (car e)))
                     (popl-error "Ill formed syntax: " expr))) binding-lst)))

;helper function. Called recersively so nested lets can be made.
(define (let*-helper bindlst body)
    (if (null? (cdr bindlst))
        ;if the cdr is null, we do not need to call the fn recersively
        ;also, the format of the list differs slightly so that the body can be incorporated properly
        (append (cons 'let (list (list (car bindlst)))) body)
        ;recersively making nested lets
        (append (cons 'let (list (list (car bindlst))))
            (list (let*-helper (cdr bindlst) body)))))

;converts let* into nested lets
(define (popl-eval-let* expr env)
    (proper-let expr) ; checking to make sure the let* is formatted properly
    (popl-eval (let*-helper (cadr expr) (cddr expr)) (popl-copy-environment env)))

;given a list in the form ((a b) (c d) ...)
;returns a list in the form (a c ...)
(define (get-cars lst)
    (map (lambda (e) (car e)) lst))

;given a list in the form ((a b) (c d) ...)
;returns a list in the form (b d ...)
(define (get-cadrs lst)
    (map (lambda (e) (cadr e)) lst))

;evaluates the let arguments if when given a list
(define (popl-eval-let-args lst env)
    (if (null? lst)
        ()
        (if (list? (car lst))
            (append (list (popl-eval (car lst) env)) (popl-eval-let-args (cdr lst) env))
            (append (list (car lst)) (popl-eval-let-args (cdr lst) env)))))

;converts the let into a lambda and calls popl-eval on the lambda
;retored and removes any new bindings made in the let expression
;ex. (let ((x 2) (y 3))
;         (x * y))
;convert to:
;((lambda (x y) (* x y)) 2 3)
(define (popl-eval-let expr env)
    (proper-let expr)

        (let ((vars (get-cars (cadr expr)))
              (copy-env (popl-copy-environment env)))

             ;lambda expression made and evaluated
            (popl-eval (append
                (list (append (cons 'lambda (list vars)) (cddr expr)))
                    (popl-eval-let-args (get-cadrs (cadr expr)) copy-env)) copy-env)))

;gets the last element of a list
(define (get-last lst)
    (if (null? (cdr lst))
        (car lst)
        (get-last (cdr lst))))

;; given a non-primitive function,
;; make a copy of the function's environment
;; and with that copy,
;; 1. bind each parameter to corresponding
;;    arguments,
;; 2. evaluate each
;; element of the function's body.
;; 3. Return the last value.
(define (popl-apply function arguments)
    (for-each (lambda (p a) (popl-bind p a (fourth function)))
        (cadr function)
        arguments)

        (let ((env  (fourth function))
              (function-body (third function)))

            ;evaluating elements of the function's body and returning the last value
            (get-last (map (lambda (e) (popl-eval e (popl-copy-environment env))) function-body))))

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

  (cond
      ;checking for an unequal number of arguments and parameters
      ((and (list? fun) (eq? (car fun) *LAMBDA*) (not (= (list-length args) (list-length (cadar all-evaluated)))))
        (popl-error "Function expected " (list-length (cadar all-evaluated)) "arguments, but " (list-length args) " given"))
      ;if it is a non-primative expressions
      ((and (list? fun) (eq? (car fun) *LAMBDA*))
       (popl-apply fun args))
      ;if it is a primative expression
      ((procedure? (car all-evaluated))
        (apply fun args))
      ;otherwise, it is an error.
      (else (popl-error "The object " (popl-eval (car expr) env) " is not applicable.")))))

;given a list of symbols, determines if there are any repeated symbols.
;Returns #t if there are repeats, returns #f if there are not
(define (unique-symbols symbol-list)
    (or (null? symbol-list)
        (and (symbol? (car symbol-list))
             (not (memq (car symbol-list) (cdr symbol-list)))
             (unique-symbols (cdr symbol-list)))))

;add syntax checking and errors
;ex if we write (lambda) is bad or (lambda ())
(define (popl-eval-lambda expr env)
   ;; expr is something like:  (lambda (a b c) form1 form2)
   ;; Error checking needs to be added to this function ensuring
   ;; that the parameter list is a list containing only
   ;; symbols with no duplicates.

   ;cond statement checking for different errors in lambda formation. Error seperated for clarity.
   (cond ((< (list-length expr) 3)
      (popl-error "Ill formed syntax: " expr))
      ;if the parameter list is not a list. Error.
      ((not (list? (second expr)))
       (popl-error "Ill formed syntax: " expr))
      ;if there are repeats in symbols for the parameter list, error
      ((not (unique-symbols (second expr)))
       ((popl-error "Ill-formed special form: " expr))))

    ;checking that all of the elements in the parameter list are symbols
    (for-each (lambda (e) (if (not (symbol? e)) (popl-error "Ill formed syntax: " expr))) (second expr))

   (list *LAMBDA*             ;; special object indicating it's a lambda
         (second expr)        ;; parameter list
         (cddr expr)          ;; function body (list of expressions)
         env))                ;; the current environment

;determines the length of lst
(define (list-length lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst)))))

;; The name of the main evaluator is popl-eval-help
;; so that we can do enhanced tracing. See further comments below.
(define (popl-eval-help expr env)
  (cond ((or (number? expr)
             (boolean? expr)
             (string? expr)
             (null? expr)) expr)
        ;If the expression is a symbol, check to see if there is an existing binding for it.
        ;If so, return the binding. If there is no binding, signal an error
        ((symbol? expr) (if (eq? #f (popl-get-binding expr env))
                            (popl-error "Unbound variable: " expr)
                            (popl-env-value expr env)))
        ((pair? expr)
         (cond ((eq? (first expr) 'define)
                ;checking to see if expr is in the proper form for define
                (if (or (< (list-length expr) 3) (> (list-length expr) 3))
                    (popl-error "Ill-formed special form: " expr)
                    (let ((sym (second expr))
                        (val (popl-eval (third expr) env)))
                        (popl-bind sym val env))))
               ((eq? (first expr) 'quote)
                ;checking to see if expr is in the proper form for quote
                (if (= (list-length expr) 2)
                    (second expr)
                    (popl-error "Ill formed syntax: " expr)))
               ((eq? (first expr) 'lambda)
                (popl-eval-lambda expr env))
               ((eq? (first expr) 'if)
                (popl-eval-if expr env))
               ((eq? (first expr) 'cond)
                (popl-eval-cond expr env))
               ((eq? (first expr) 'set!)
                (popl-eval-set! expr env))
               ((eq? (first expr) 'let)
                (popl-eval-let expr env))
               ((eq? (first expr) 'let*)
                (popl-eval-let* expr env))
               (else (popl-eval-function-call expr env))))
        (else (popl-error "(internal) unknown object " expr " passed to evaluator"))))

;; Change this to #f before submitting:
(define *ENABLE-DEBUG* #f)

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

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
(popl-bind '* * *TOPENV*)
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

; do i need error checking?
(define (popl-eval-if expr env)
        (if (popl-eval (second expr) env)
            (popl-eval (third expr) env)
            (popl-eval (fourth expr) env)))

;expr is in the form
;(cond (cond1
;       then1)

;      (cond2
;       then2)

;      (cond3
;       then3)

;      (else
;       action4))

(define (popl-eval-cond expr env)
    (if (not (null? (cdr expr)))
        (popl-eval-cond-helper (cdr expr) env)
    )
)

(define (popl-eval-cond-helper expr env)
    (if (not (pair? (car expr))) (popl-error "Ill formed cond statment.")
        (if (not (null? expr))
            (cond ((popl-eval (caar expr) env) (popl-eval (cadar expr) env))
                  ((not (null? (cdr expr))) (popl-eval (popl-eval-cond-helper (cdr expr) env) env))
             )
        )
    )
)

(define (popl-eval-set! expr env)
    ;if there isn't a current binding, signal an error.
    (if (not (popl-get-binding (cadr expr) env))
        (popl-error "Unbound variable " (cadr expr))
    )
    (let ((var (popl-env-value (cadr expr) env)))
         ;updates the current value of the variable
         (popl-bind (cadr expr) (caddr expr) env)
         ;returns the previous value of the variable
          var
    )
)

;helper function. Called recersively so nested lets can be made.
(define (let*-helper bindlst body)
    (if (null? bindlst)
        body
        (append (cons 'let (list (list (car bindlst))))
            (list (let*-helper (cdr bindlst) body)))
        )
)

;converts let* into nested lets
(define (popl-eval-let* expr env)
    (popl-eval (let*-helper (cadr expr) (caddr expr)) env)
)

;given a list, check if it is the form ((a b) (c d) ...)
;returns a list in the form (a c ...)
(define (get-cars lst)
    (if (null? lst) ()
        (if (not (null? (caar lst)))
        (cons (caar lst) (get-cars (cdr lst)))))
)

;given a list, check if it is the form ((a b) (c d) ...)
;returns a list in the form (b d ...)
(define (get-cadars lst)
    (if (null? lst) ()
        (if (not (null? (cadar lst)))
        (cons (cadar lst) (get-cadars (cdr lst)))))
)

(define (cars-are-lists expr)
    (if (null? expr)
        #t
        (if (list? (car expr))
            (cars-are-lists (cdr expr))
            #f
        )
    )
)

; Note: currently does not work for: (let ((x (+ x 1))) (* x 2)) -> ((lambda (x) (* x 2)) (+ x 1))

;(let ((x 2) (y 3))
;    (x * y))

;convert to
;((lambda (x y) (* x y)) 2 3)
(define (popl-eval-let1 expr env)
    (if (or (null? (cdr expr)) (null? (cddr expr)) (not (cars-are-lists (cadr expr))))
        (popl-error "Ill formed syntax")
        ;might need to evaluate cadrs if they are in form ex (+ 2 1)
        (popl-eval (append (list (append (cons 'lambda (list (get-cars (cadr expr)))) (cddr expr))) (get-cadars (cadr expr))) env)
    )
)

(define (popl-eval-let-args lst env)
    (if (null? lst)
        ()
        (if (list? (car lst))
            (append (list (popl-eval (car lst) env)) (popl-eval-let-args (cdr lst) env))
            (append (list (car lst)) (popl-eval-let-args (cdr lst) env))
        )
    )
)
;not finished
(define (popl-eval-let expr env)
    (if (or (null? (cdr expr)) (null? (cddr expr)) (not (cars-are-lists (cadr expr))))
        (popl-error "Ill formed syntax")
        ;might need to evaluate cadrs if they are in form ex (+ 2 1)

        ;(popl-eval-let-args (get-cadars (cadr expr)) env)
        (popl-eval (append
            (list (append (cons 'lambda (list (get-cars (cadr expr)))) (cddr expr)))
                (popl-eval-let-args (get-cadars (cadr expr)) env)) env)
    )
)
;; given a non-primitive function,
;; make a copy of the function's environment
;; and with that copy,
;; 1. bind each parameter to corresponding
;;    arguments,
;; 2. evaluate each
;; element of the function's body.
;; 3. Return the last value.

;check if apply is right
(define (popl-apply function arguments env)
(for-each (lambda (p a) (popl-bind p a env))
    (cadr function)
    arguments)
    (popl-eval (third function) env) ;uncomment when this is working for primitives
    ;function
 )

;; Evaluate all the elements of expr,
;; which is a list.
;; If the first element of the list is a procedure
;;   (primitive function), then use scheme's apply
;;   function.
;; Else if the first element is a non-primitive function
;;   (begins with *LAMBDA*), then use popl-apply.
;; Otherwise use popl-error.

;decide if procedure given is primative or not
(define (popl-eval-function-call expr env)
    (if (procedure? (first expr))
        (popl-eval-primative expr env)
        (if (eq? (car (first expr)) 'lambda)
            (popl-apply (first expr) (cdr expr) env)
            (popl-error "The function call is in the wrong form")
        )
    )
)

(define (popl-eval-primative expr env)
   (let* ((all-evaluated (map (lambda (e) (popl-eval e env)) expr))
          (fun (car all-evaluated))
          (args (cdr all-evaluated)))
      (apply fun args))
      )

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

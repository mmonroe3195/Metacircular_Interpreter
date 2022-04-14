(define (let-demo3 x)
    (let ((x (+ x 1)))
        (let ((y (- x 1)))
            (* x y))))

(define (let-demo3a x)
    (let* ((x (+ x 1))
          (y (- x 1)))
          (* x y)))

(define (let-helper bindlst body)
    (if (null? bindlst)
        body
        (append (cons 'let (list(list (car bindlst))))
            (list (let-helper (cdr bindlst) body)))
        )
)
(define (popl-eval-let* expr)
    (let-helper (cadr expr) (caddr expr))
)

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

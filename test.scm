(define (let-demo3 x)
    (let ((x (+ x 1)))
        (let ((y (- x 1))))
            (* x y)))

(define (let-demo3a x)
    (let* ((x (+ x 1))
          (y (- x 1)))
          (* x y)))

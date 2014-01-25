

(define (foo)
  (let ((x 20))
    'x))


(let ((x 100))
  (eval (foo)))




(define (print x)
  (display (format "~a\n" x)))


(define av #t)
(define bv #f)


(define (a k) `(,k ,av))

(define (b k) `(,k ,bv))

(define (ae k) `(,k "AE"))

(define (be k) `(,k "BE"))

(define (c k) `(,k "C"))

(define (x k)
  (a (lambda (v) (if v (ae k)
		     (b (lambda (v) (if v (be k)
					(c k))))))))

(let again ((y (x '())))
  (print y)
  (if (not (null? (car y)))
      (again (apply (car y) (cdr y)))))


;; this is a trampoline similar to the following


(cond
 (av "AE")
 (bv "BE")
 (else "C"))

;;

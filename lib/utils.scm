(define (println x) (display x) (newline))

(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append #nil (map proc seq)))

(define (reduce op l)
  (accumulate op (car l) (cdr l)))

(define (get-exec-time p) 
  (define start (get-internal-real-time))
  (define expected-res (p))
  (define end (get-internal-real-time))
  (newline)(display (/ (- end start) 1e6)) (display " ms")(newline)
  expected-res)

(define (repeat p n) 
  (if 
    (not (eq? n 0))
    (begin (p) (repeat p (- n 1)))))

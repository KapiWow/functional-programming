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

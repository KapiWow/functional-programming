(load "../lib/utils.scm")
(load "../lib/math.scm")


;(define (delay a) (lambda () a))
;(define (force a) (a))


(define (stream-car stream)
    (car stream))

(define (stream-cdr stream)
    (force (cdr stream)))

;; cons-stream needs to be a macro

(define-syntax cons-stream
    (syntax-rules ()
          [(_ a b) (cons a (delay b))]
              ))

(define the-empty-stream '())

(define stream-null? null?)

;; define stream-for-each
;; run a procedure on each element of stream
(define (stream-for-each proc stream)
    (if (stream-null? stream)
            'done
                  (begin (proc (stream-car stream))
                                      (stream-for-each proc (stream-cdr stream)))))

(define test-stream (cons-stream 2 (cons-stream 3 (cons-stream 4 '()))))

;; displays passed argument but with a newline

(define (display-stream stream)
    (stream-for-each println stream))

;; stream map
(define (stream-map proc stream)
    (if (stream-null? stream)
            the-empty-stream
                  (cons-stream (proc (stream-car stream))
                                                  (stream-map proc (stream-cdr stream)))))

(define (sqr x)
    (* x x))

(define (stream-enumerate-interval low hi)
    (if (> low hi)
            '()
                  (cons-stream low (stream-enumerate-interval (+ 1 low) hi))))

(define (shower x)
    (println x)
      x)

;; this works
(define yyy (stream-map sqr (stream-enumerate-interval 1 10)))


(define (stream->list stream)
    (if (stream-null? stream)
            '()
                  (cons (stream-car stream)
                                    (stream->list (stream-cdr stream)))))

; ==> (1 4 9 16 25 36 49 64 81 100)

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                        pred
                        (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))


(display (stream->list yyy)) (newline)

(display-stream (stream-filter prime?
                               (stream-enumerate-interval
                                 10 100)))
(newline)


(define (show x)
  (println x)
  x)


(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(println "next command")
(stream-ref x 7)


(println "=============================")
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(display "sum: ")
(println sum)
(define y (stream-filter even? seq))
(display "sum: ")
(println sum)
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))

(display "sum: ")
(println sum)
(println (stream-ref y 7))
(display "sum: ")
(println sum)
(display-stream z)

(display "sum: ")
(println sum)

;delay и force запоминают результаты вычислений

(println "=============================")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(println (stream-ref integers 10))

(println "=============================")

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))


(define (stream-print-first s n) 
    (if (= n 0) 
      (newline)
      (begin (display (stream-car s)) (display " : ")
       (stream-print-first (stream-cdr s) (- n 1)))))

(stream-print-first no-sevens 20)

(println "=============================")

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(stream-print-first fibs 10)

(println "=============================")

;по очереди убираем каждое число, которое делится на текущее
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(stream-print-first primes 10)

(println "=============================")

(define ones (cons-stream 1 ones))
(stream-print-first ones 10)

;;(define (stream-map proc . streams)  
;;  (if (any? stream-null? streams)  
;;    empty-stream 
;;    (stream-cons 
;;      (apply proc (map stream-car streams)) 
;;      (apply stream-map  
;;             proc
;;             (map stream-cdr streams))))) 

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))


(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(stream-print-first integers 10)


(println "=============================")

(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

;            1 : 1 : 2 : 3 : 5 : 8  : 13 : 21 : 34
;            0 : 1 : 1 : 2 : 3 : 5  : 8  : 13 : 21 : 34
;    0 : 1 : 1 : 2 : 3 : 5 : 8 : 13 : 21 : 34

(stream-print-first fibs 10)


(println "=============================")

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream double 2)))

(stream-print-first double 10)

(println "=============================")


(define primes
  (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))


(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(stream-print-first primes 10)


(println "=============================")

(define s (cons-stream 1 (add-streams s s)))

(stream-print-first s 10)

(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

(define (mul-streams s1 s2) (stream-map * s1 s2))

(stream-print-first factorials 10)

(println "=============================")

;(define (partial-sums s)
;  (cons-stream 1 (add-streams (stream-cdr s) factorials)))



(define (partial-sums s)
  (if (stream-null? s)
    0
    (cons-stream (stream-car s)
                 (stream-map (lambda (x) (+ x (stream-car s)))
                             (partial-sums (stream-cdr s))))))

(stream-print-first (partial-sums integers) 10)


(println "=============================")

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)
(stream-print-first (sqrt-stream 2) 10)


(println "=============================")

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(stream-print-first pi-stream 20)


(println "=============================")

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)
;почему это хуже, чем предыдущее определение?
(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
                     (lambda (guess)
                       (sqrt-improve guess x))
                     (sqrt-stream x))))


(println "=============================")


(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))


(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))

(define prime-sum (stream-filter
  (lambda (pair) (prime? (+ (car pair) (cadr pair))))
  int-pairs))

(stream-print-first prime-sum 20)

(stream-print-first int-pairs 20)


(println "=============================")




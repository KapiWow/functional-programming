(load "../lib/utils.scm")

(define-syntax cons-stream
    (syntax-rules ()
          [(_ a b) (cons a (delay b))]
              ))
(define (stream-car stream)
    (car stream))
(define (stream-cdr stream)
    (force (cdr stream)))
(define the-empty-stream '())
(define stream-null? null?)

(define (stream-for-each proc stream)
  (if (stream-null? stream)
    'done
    (begin (proc (stream-car stream))
           (stream-for-each proc (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define (display-stream stream)
    (stream-for-each println stream))

(define (stream-enumerate-interval low hi)
  (if (> low hi)
    the-empty-stream
    (cons-stream low 
                 (stream-enumerate-interval (+ 1 low) hi))))

(define (stream->list stream)
  (if (stream-null? stream)
    '()
    (cons (stream-car stream)
          (stream->list (stream-cdr stream)))))

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

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (stream-print-first s n) 
  (if (= n 0) 
    (newline)
    (begin (display (stream-car s)) (display " : ")
           (stream-print-first (stream-cdr s) (- n 1)))))

(define ones (cons-stream 1 ones))
(define zeros (cons-stream 0 zeros))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (mul-streams s1 s2) (stream-map * s1 s2))

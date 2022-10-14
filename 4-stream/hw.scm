(load "../lib/utils.scm")
(load "../lib/math.scm")
(load "../lib/stream.scm")

(define-syntax cons-stream
    (syntax-rules ()
          [(_ a b) (cons a (delay b))]
              ))

;task1
;напишите функцию stream-random, которая создает поток с случайными числами от 0 до n. 
;Можете воспользоваться стандартной функцией (random x)
;Пользуясь stream-random выведете на экран 10 нечетных случайных чисел

(println "==================")

;task2
;создайте поток потоков, который содержит следующую матрицу:
;1 : 2 : 3 : 4 : 5
;2 : 3 : 4 : 5 : 6
;3 : 4 : 5 : 6 : 7
;4 : 5 : 6 : 7 : 8
;5 : 6 : 7 : 8 : 9
;6 : 7 : 8 : 9 : 10
;т.е. на позиции (i,j) стоит (i+j+1) элемент (при условии, что i,j начинаются с нуля)
;выведете на экран матрицу 10 на 10
;Нельзя использовать stream-car, stream-cdr, cons-stream

(println "==================")

;task3
;напишите аналог accumulate и reduce для потоков
;(define (stream-accumulate op initial stream) ???)

;(define (stream-reduce op stream) ???)

(define y (stream-enumerate-interval 0 10))
;раскомментируйте тесты
;(println (stream-accumulate + 0 y)) ;55
;(println (stream-reduce + y)) ;55

(println "==================")

;task4
;Напишите функцию, которая создаст поток, который является объединением двух потоков
;(define (stream-unite s1 s2) ???)
;элементы в конечном потоке должны чередоваться

;раскомментируйте тесты
;(stream-print-first (stream-unite ones zeros) 10)
; 1 : 0 : 1 : 0 : 1 : 0 : 1 : 0 : 1 : 0 :

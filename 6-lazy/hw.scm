(load "../lib/utils.scm")
(load "../lib/lazy.scm")

;2 балла
;Задание 1
;Модифицируйте функцию text-of-quotation, чтобы она поддерживала quoted list(см. тест)

;(define (text-of-quotation-lazy exp) ???)

(define text-of-quotation text-of-quotation-lazy)

;(println (`',+ 1 2))
;в задании вам пригодятся функции quasiquote, unquote (они же ` ,)
;https://docs.racket-lang.org/reference/quasiquote.html
(println (quasiquote(0 1 2)))
(println `(0 1 2)) 
;'(0 1 2)
(println (quasiquote (0 (unquote (+ 1 2)) 4)))
(println `(0 ,(+ 1 2) 4))
;'(0 3 4)

;test
(define prog '(
  (car '(a b c))
))
(run-prog prog)
;a

;2 балла
;Задание 2
;Модифицируйте функцию таким образом, чтобы она поддерживала вложенные списки

;tests
(define prog '(
  (car (cdr '(a b c)))
))
(run-prog prog)
;b

(define prog '(
  (car (car '((a b) (c (d e))))) 
))
(run-prog prog)
;a

(define prog '(
  (car (cdr (car '((a b) (c (d e))))))
))
;b
(run-prog prog)

(define prog '(
  (car (car (cdr '((a b) (c (d e))))))
))
;c
(run-prog prog)
(define prog '(
  (car (car (car '(((a b))))))
))
;a
(run-prog prog)
(define prog '(
  (car(cdr (car (car '(((a b)))))))
))
;b
(run-prog prog)
(define prog '(
 (car (car '((()))))
))
;()

(run-prog prog)

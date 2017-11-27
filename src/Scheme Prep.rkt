#lang racket

;1
(define first (lambda (x) (car x)))
(define second (lambda (x) (cadr x)))
(define third (lambda (x) (caddr x)))
(define forth (lambda (x) (cadddr x)))
(define fifth (lambda (x) (car(cddddr x))))
(define family '(josh sara erin sandy jon))
(first family)
(second family)
(third family)
(fourth family)
(fifth family)

;2
(define (truecount alist)(count identity alist))

(truecount '(#t #t #f #f #t))

;3
(define (squarelist alist)(map (lambda (x) (* x x)) alist))

(squarelist '(1 2 3 4 5))

;4
(define (hundreds? alist)(filter (lambda (x) (> x 100)) alist))

(hundreds? '(312 99 502 18 232 56 7))

;5
(define (collatz n)
  (cond ((eq? n 1) '())
        ((odd? n) (cons (+ (* n 3) 1) (collatz (+ (* n 3) 1))))
        ((even? n) (cons (/ n 2) (collatz (/ n 2))))))

(collatz 69)
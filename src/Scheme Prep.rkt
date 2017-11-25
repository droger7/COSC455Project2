#lang racket

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

(define truecount (count identity '(#f #f #t #t #f #t)))

truecount

(define (squarelist alist)(map (lambda (x) (* x x)) alist))

(squarelist '(1 2 3 4 5))

(define hundreds?(filter (lambda (x) (> x 100)) '(312 9 502 8 232 4 7)))

hundreds?


(define (collatz num)
  (cond ((= num 1) '())
        ((odd? num) (cons (+ (* num 3) 1) (collatz (+ (* num 3) 1))))
        ((even? num) (cons (/ num 2) (collatz (/ num 2))))))

(collatz 9)
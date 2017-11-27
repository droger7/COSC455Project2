#lang racket

(define chinese '(ling yi er san si wu liu qi ba jiu shi))
(define english '(zero one two three four five six seven eight nine ten))

(define (go alist)
  (display "Translation: ") (display(trans alist)) (newline)
  (display "Addition: ") (display(add-between (trans alist) "+")) (display " = ") (display (sum (trans alist))) (newline)
  (display "Multiplication: ") (display(add-between (trans alist) "*")) (display " = ") (display (mult (trans alist))) (newline)(newline)
  )

(define (trans alist)
  (cond ((null? alist) null)
        ((member? (car alist) english)(cons (index-of english(car alist)) (trans(cdr alist))))
        ((member? (car alist) chinese)(cons (index-of chinese(car alist)) (trans(cdr alist))))
        (else (trans(cdr alist)))
        )
  )

(define (member? item alist)
  (sequence-ormap (lambda (x)
                    (equal? item x))
                     alist))

(define sum
  (lambda (the-list)
    (apply + the-list)))

(define mult
  (lambda (the-list)
    (apply * the-list)))

(go '(yi nine josh six josh ba))
(go '(yi josh three si))
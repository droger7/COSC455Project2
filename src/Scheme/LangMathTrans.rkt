#lang racket

(define chinese '(ling yi er san si wu liu qi ba jiu shi))
(define english '(zero one two three four five six seven eight nine ten))

(define (go alist)
  (display "Translation: ") (display(construct alist)) (newline)
  (display "Addition: ") (display(add-between (construct alist) "+")) (display " = ") (display (foldl + 0 (construct alist))) (newline)
  (display "Multiplication: ") (display(add-between (construct alist) "*")) (display " = ") (display (foldl * 1 (construct alist))) (newline)(newline)
  )

(define (construct alist)
  (cond ((null? alist)'())
        ((member (car alist) chinese)(cons (index-of chinese(car alist)) (construct(cdr alist))))
        ((member (car alist) english)(cons (index-of english(car alist)) (construct(cdr alist))))
        (else (construct(cdr alist)))
        )
  )

(go '(yi josh three si))
(go '(yi nine josh six ba))


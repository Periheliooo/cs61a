(define (ascending? s)
    (cond ((or (null? s) (null? (cdr s)))  #t)
            ((<= (car s) (car (cdr s))) (ascending? (cdr s)))
            (else #f))
    )

(define (my-filter pred s)
    (if (not (null? s)) (append(if (pred (car s)) (list (car s)) nil) (my-filter pred (cdr s))) nil
    ))

(define (interleave lst1 lst2)
    (
        cond ((null? lst1) lst2) 
            (else (append (list (car lst1)) (interleave lst2 (cdr lst1))))
    ))

(define (no-repeats s)
    (
        cond ((null? s) nil)
            (else (cons (car s) (no-repeats (filter (lambda (x) (not (= x (car s)))) (cdr s)))))
    ))
 
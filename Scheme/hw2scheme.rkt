#|
Geon Kang
N17120399
Programming Languages
HW2: Scheme programming part
|#

;Problem 1.
;a)increment:
(define (inc n)
  (+ n 1))
;b)decrement:
(define (dec n)
  (- n 1))

;lambda implementations for practice
(define inclambda (lambda (x) (+ x 1)))
(define declambda (lambda (x) (- x 1)))



;Problem 2.
(define mulcur (lambda (x) (lambda (y) (* x y))))


;Problem 3.

(define (compose fun1 fun2)
  (lambda (x) (fun1 (fun2 x))))


;Problem 4.

#| Ada cannot handle the program on slide 29 because Ada has "static checks to reject possible dangling references" (Slide 30 of subprogram lecture)
  Thus, First and Second in Function Result cannot be dereferenced because they exist in the enclosing scope of Function Compose.
  Furthermore, static chains do not work with function values so First(x) will not be able to be passed to Second.
  Scheme does not have the above mentioned restrictions. Functions and function values can be passed to functions, allowing compose
  to simply be defined as (Second(First x))
|#



;Problem 5.
                                                                                                                           
;basic structure of map code taken from slide 36 of Lambda Calculus & Scheme Lecture
(define (map2 lis1 lis2 p fun)
  ;keyword "length" obtained from scheme standard link in course documents 
  (if (equal? (length lis1)(length lis2))
      (cond
        ((null? lis1) '())
        ((null? lis2) '())
      (else (cond
        ((p (car lis1)) (cons (fun(car lis2))(map2 (cdr lis1) (cdr lis2) p fun)))
        (else (cons (car lis2)(map2 (cdr lis1) (cdr lis2) p fun)))
            )
       )
       )
      "lists are not of equal size"
  )
)


;Problem 6.
(define (skip n)
  (if (equal? n 0)
      (lambda(x) x)
      (lambda (x)(skip (- n 1)))
  )
)




;Problem 7.

#| 
Referenced "3.5 Proper tail recursion" from Scheme Standard

the last call to (a) is the only tail call in the code so we are only interested in whether (a) is a recursive call 
 (ie. (a) bound to the lambda function itself)

But if (a) is bound to the lambda function itself, (if (a)) will keep calling the lambda function to try and evaluate the test condition
and the tail call will never be reached.

Thus, the code is not tail recursive.

|#
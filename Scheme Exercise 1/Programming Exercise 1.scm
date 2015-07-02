; Sarah Whelan
; slw96
; 1/31/15
; Programming Exercise 1

; 1. multiplyby: multiplies a list by a number returning a list of all the numbers in the list multiplied by the specified number
(define multiplyby
  (lambda (n l)
    (cond
      ((null? l) '())
      ((number? (car l)) (cons (* n (car l)) (multiplyby n (cdr l))))
      (else (cons (car l) (multiplyby n (cdr l)))))))

; 2. maxnumber: returns the maximum value of a list of numbers that can contain letters and negatives but must contain at least one number 
(define maxnumber
  (lambda (l)
    (cond
      ((null? (cdr l)) (car l))
      ((and (number? (car l))(> (car l) (maxnumber (cdr l)))) (car l))
      (else (maxnumber (cdr l))))))

; 3. removelast: returns the given list without its last element
(define removelast
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) '())
      (else (cons (car l) (removelast (cdr l)))))))

; 4. crossmultiply: takes two lists of numbers and returns a list of lists where each list is every number in the second list multiplied by one of the numbers in the first list
(define crossmultiply
  (lambda (l1 l2)
    (cond
      ((or(null? l1)(null? l2)) '())
      (else (cons (multiplyby (car l1) l2)(crossmultiply (cdr l1) l2))))))


; 5. interleave3: takes three lists and interleaves them returning the first element of l1, l2, l3 then the second element of l1, l2, l3 etc until all elements are in the new list
(define interleave3
  (lambda (l1 l2 l3)
    (cond
      ((and (null? l1) (null? l2) (null? l3)) '())
      ((null? l1) (interleave3 l2 l3 '()))
      (else (cons (car l1) (interleave3 l2 l3 (cdr l1)))))))


; 6. reverse*: takes a list (may include sublists) and reverse every element in the list and the elements of the sublists
(define reverse*
  (lambda (l)
    (cond
      ((null? l) l)
      ((list? (car l)) (append (reverse* (cdr l)) (cons(reverse* (car l)) '())))
      (else (append (reverse* (cdr l)) (cons (car l) '()))))))

; 7. reverelists: takes a list (may include sublists) and reverses it if there are sublists the elements of the sublists are not reversed 
; but if the sub list contains a list the elements in the sub-sub-list are revered etc
(define reverseHelp
  (lambda (l)
    (cond
      ((null? l) l)
      ((list? (car l)) (append (reverseHelp (cdr l)) (cons(reverselists(car l))'())))
      (else (append (reverseHelp(cdr l)) (cons(car l) '()))))))

(define reverselists
  (lambda (l)
    (cond
      ((null? l) l)
      ((list? (car l)) (cons (reverseHelp(car l)) (reverselists(cdr l))))
      (else (cons (car l) (reverselists (cdr l)))))))

; 8. trimatoms: given a list (may contain sublists) and a list of atoms returns the list of atoms without the first k elements where k is the number of non-null atoms in the first list
(define trimatoms
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      ((null? l2) '())
      ((null? (car l1)) (trimatoms (cdr l1) l2))
      ((list? (car l1)) (trimatoms (cdr l1)(trimatoms (car l1) l2)))
      (else (trimatoms (cdr l1) (cdr l2))))))
      
; 9. partialsums*: returns the sum the list and the sum of any sublists in the list in the same structure as the list. The list may contain non-numbers.
(define partialsums*
  (lambda (l)
    (cond
      ((null? l) (cons 0 '()))
      ((list? (car l)) (cons (+ (car (partialsums* (car l))) (car (partialsums* (cdr l))))(cons (partialsums* (car l)) (cdr (partialsums* (cdr l))))))
      ((number? (car l)) (cons (+ (car l) (car (partialsums* (cdr l)))) (cdr (partialsums* (cdr l)))))
      (else (partialsums* (cdr l))))))

; 10. exchange: takes two lists (the first is allowed to have sublists) and returns a list with the same structure as the first list but with the atoms from the second list
(define exchange
  (lambda (l1 l2)
    (cond
      ((or (null? l1) (null? l2)) '())
      ((list? (car l1)) (cons (exchange (car l1) l2) (exchange (cdr l1) (trimatoms (car l1) l2))))
      (else (cons (car l2) (exchange (cdr l1) (cdr l2)))))))
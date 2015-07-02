; Sarah Whelan
; slw96
; 2/20/2015
; Programming Assignment 2
; EECS 345 PLC CWRU

; 1. multiplyby: takes a number and a list and returns a list of the items in the list multiplied by the number

(define multiplyby
  (lambda (n l)
    (multiplyby-cps n l (lambda (v) v))))

(define multiplyby-cps
  (lambda (n l return)
    (cond
      ((null? l) (return '()))
      ((number? (car l)) (multiplyby-cps n (cdr l) (lambda (v) (return (cons (* n (car l)) v)))))
      (else (multiplyby-cps n (cdr l) (lambda (v) (return (cons (car l) v))))))))

; 2. crossmultiply: takes two lists and returns a list of lists where each sublist contains the second list multiplied by a number in the first list

(define crossmultiply
  (lambda (l1 l2)
    (crossmultiply-cps l1 l2 (lambda(v) v))))

(define crossmultiply-cps
  (lambda (l1 l2 return)
    (cond
      ((or(null? l1)(null? l2)) (return '()))
      (else (crossmultiply-cps (cdr l1) l2 (lambda (v) (multiplyby-cps (car l1) l2 (lambda (u) (return (cons u v))))))))))
  
; 3. max number: takes a list of atoms and numbers and returns the largest value

(define maxnumber
  (lambda (l)
    (maxnumber-cps l (lambda(v) v))))

(define maxnumber-cps
  (lambda (l return)
    (cond
      ((null? (cdr l)) (return (car l)))
      ((number? (car l))(maxnumber-cps (cdr l) (lambda (v) 
                                                 (if (< (car l) v)
                                                     (return v)
                                                     (return (car l))))))
      (else (maxnumber-cps (cdr l) return)))))

; 4. partialsums: takes a list (can contain sublists) and returns the sum of all of the numbers in the list
; followed by the sum of each sublist (maintaining the list structure)

(define partialsums*
  (lambda (l)
    (partialsums*-cps l (lambda (v) v))))
     
(define partialsums*-cps
  (lambda (l return)
    (cond
      ((null? l) (return '(0)))
      ((list? (car l)) (partialsums*-cps (car l) (lambda (v1) 
                                                   (partialsums*-cps (cdr l) (lambda (v2) 
                                                                               (return (cons (+ (car v1) (car v2)) (cons v1 (cdr v2)))))))))
      ((number? (car l)) (partialsums*-cps (cdr l) (lambda (v) 
                                                     (return (cons (+ (car l) (car v)) (cdr v))))))
      (else (partialsums*-cps (cdr l) return)))))                                                             

; 5. trimatoms: takes two lists, a list of atoms and a list that can contain sublists
; and returns the list of atoms minus the first n atoms where n is the number of non null elements in the second list

(define trimatoms
  (lambda (l1 l2)
    (trimatoms-cps l1 l2 (lambda (v) v))))

(define trimatoms-cps
  (lambda (l1 l2 return)
    (cond
      ((null? l1) (return l2))
      ((list? (car l1)) (trimatoms-cps (car l1) l2 (lambda (v1) (trimatoms-cps (cdr l1) v1 return))))
      (else (trimatoms-cps (cdr l1) (cdr l2) return)))))

; 6. exchange: takes two lists, a list of atoms and list possibly containing sublists
; and returns the atoms of the first list in the structure of the second list

(define exchange
  (lambda (l1 l2)
    (exchange-cps l1 l2 (lambda (v) v))))         

(define exchange-cps
  (lambda (l1 l2 return)
    (cond
      ((or (null? l1) (null? l2)) (return '()))
      ((list? (car l1)) (trimatoms-cps (car l1) l2 
                                       (lambda (trimV) (exchange-cps (cdr l1) trimV 
                                                                     (lambda (cdrV) (exchange-cps (car l1) l2 
                                                                                                  (lambda (carV) (return (cons carV cdrV)))))))))
      (else (exchange-cps (cdr l1) (cdr l2) (lambda (v) (return (cons (car l2) v))))))))

; 7. removesubsequence: takes a list of atoms/numbers and a list that may contain sublists
; and returns the second list with the first occurance of the subsequence removed 

(define removesubsequence*
  (lambda (l1 l2)
    (removesubsequence*-cps l1 l2 (lambda (v1 v2) v2))))

(define removesubsequence*-cps
  (lambda (l1 l2 return)
    (cond
      ((or (null? l1) (null? l2)) (return l1 l2))
      ((list? (car l2)) (removesubsequence*-cps l1 (car l2) (lambda (v1 v2)
                                                              (removesubsequence*-cps v1 (cdr l2) (lambda (v3 v4)
                                                                                                    (return v3 (cons v2 v4)))))))
      ((eq? (car l1) (car l2)) (removesubsequence*-cps (cdr l1) (cdr l2) return))
      (else (removesubsequence*-cps l1 (cdr l2) (lambda (v1 v2) (return l1 (cons (car l2) v2))))))))
             
; 8. split: takes a list and returns a list of two list with the elements of the input at odd indices in the first sublist an d
; elements of the input at even indices in the second sublist (the indices of input list begin at 1)

(define split
  (lambda (l)
    (split-cps l (lambda (v1 v2) (cons v1 (cons v2 '()))))))

(define split-cps
  (lambda (l return)
    (cond
      ((null? l) (return '() '()))
      (else (split-cps (cdr l) (lambda (v1 v2) (return (cons (car l) v2) v1)))))))
     
; 9. suffix: takes an atom and a list and returns the part of the list after the last occurrence of the atom
; or the whole list if the atom does not occur

(define suffix
  (lambda (x l)
    (letrec ((loop (lambda (l2 return break)
               (cond
                 ((null? l2) (return '()))
                 (else (loop (cdr l2) (lambda (v) (cond ((eq? (car l2) x) (break v))
                                                      (else (return (cons (car l2) v))))) break))))))
      (loop l (lambda (v) v) (lambda (v) v)))))

; 10. suffix: takes an atom and a list and returns the part of the list after the last occurrence of the atom
; or the whole list if the atom does not occur, uses call/cc instead of straight continuation passing style

(define suffix2
  (lambda (x l)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (l2 return)
                        (cond
                          ((null? l2) (return '()))
                          (else (loop (cdr l2) (lambda (v) (cond ((eq? (car l2) x) (break v))
                                                                 (else (return (cons (car l2) v)))))))))))
         (loop l (lambda (v) v)))))))
#lang racket

(define (atom? a)
  (not (list? a)))

(define (lat? a)
  (or (null? a)
      (and (list? a) (andmap atom? a))))

(define (member? a lat)
  (ormap (curry equal? a) lat))

(define (rember a lat)
  (if (null? lat)
      '()
      (if (eq? a (car lat)) 
          (cdr lat)
          (cons (car lat) (rember a (cdr lat))))))

(define (firsts lat)
  (if (null? lat)
      '()
      (cons (car (car lat)) 
            (firsts (cdr lat)))))

;; (define (firsts lat)
;;  (map car lat))

(define (insertR new old lat)
  (if (null? lat)
      '()
      (if (eq? old (car lat))
          (cons old (cons new (cdr lat)))
          (cons (car lat) (insertR new old (cdr lat))))))

(define (insertL new old lat)
  (if (null? lat)
      '()
      (if (eq? old (car lat))
          (cons new lat)
          (cons (car lat) (insertL new old (cdr lat))))))

(define (subst new old lat)
    (if (null? lat)
      '()
      (if (eq? old (car lat))
          (cons new (cdr lat))
          (cons (car lat) (subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
    (if (null? lat)
      '()
      (if (or (eq? o1 (car lat)) (eq? o2 (car lat)))
          (cons new (cdr lat))
          (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(define (multirember a lat)
  (if (null? lat)
      '()
      (if (eq? a (car lat))
          (multirember a (cdr lat))
          (cons (car lat) (multirember a (cdr lat))))))

(define (multiinsertR new old lat)
  (if (null? lat)
      '()
      (if (eq? old (car lat))
          (cons old (cons new (multiinsertR new old (cdr lat))))
          (cons (car lat) (multiinsertR new old (cdr lat))))))
      
(define (multiinsertL new old lat)
  (if (null? lat)
      '()
      (if (eq? old (car lat))
          (cons new (cons old (multiinsertL new old (cdr lat))))
          (cons (car lat) (multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
  (if (null? lat)
      '()
      (if (eq? old (car lat))
          (cons new (multisubst (cdr lat)))
          (cons (car lat) (multisubst new old (cdr lat))))))


;; CHAPTER 4

(define (add1 n)
  (+ 1 n))

(define (sub1 n)
  (- n 1))

(define (o+ n m)
  (if (zero? m) 
      n
      (add1 (o+ n (sub1 m)))))

(define (o- n m)
  (if (zero? m) 
      n
      (sub1 (o- n (sub1 m)))))

(define (addtup tup)
  (if (null? tup) 
      0
      (+ (car tup) (addtup (cdr tup)))))

(define (x n m)
  (if (zero? m) 
      0
      (o+ n (x n (sub1 m)))))

(define (tup+ tup1 tup2)
  (cond 
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else
     (cons (+ (car tup1) (car tup2)) 
           (tup+ (cdr tup1) (cdr tup2))))))

(define (> n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (> (sub1 n) (sub1 m)))))
  
(define (< n m)
  (cond
    ((zero? m) #f)
    ((zero? n) #t)
    (else (< (sub1 n) (sub1 m)))))

(define (= n m)
  (cond
    ((> n m) #f) 
    ((< n m) #f) 
    (else #t)))

(define (↑ n m)
  (if (zero? m) 
      1
      (x n (↑ n (sub1 m)))))

(define (pick n lat)
  (if (one? n)
      (car lat)
      (pick (sub1 n) (cdr lat))))

(define (rempick n lat)
  (if (one? n)
      (cdr lat)
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))

(define (no-nums lat)
  (filter (lambda (n) (not (number? n))) lat))

(define (all-nums lat)
  (filter number? lat))

(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2))))

(define (occur a lat)
  (if (null? lat)
      0
      (if (eq? (car lat) a)
          (add1 (occur a (cdr lat)))
          (occur a (cdr lat)))))

(define (one? n)
  (= n 1))


;; CHAPTER 5

(define (rember* a l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (if (eq? (car l) a)
         (rember* a (cdr l))
         (cons (car l) (rember* a (cdr l)))))
    (else (cons (rember* a (car l))
                (rember* a (cdr l))))))
     
(define (insertR* new old l)
   (cond
    ((null? l) '())
    ((atom? (car l))
     (if (eq? (car l) old)
         (cons old (cons new (insertR* new old (cdr l))))
         (cons (car l) (insertR* new old (cdr l)))))
    (else (cons (insertR* new old (car l))
                (insertR* new old (cdr l))))))

(define (insertL* new old l)
   (cond
    ((null? l) '())
    ((atom? (car l))
     (if (eq? (car l) old)
         (cons new (cons old (insertL* new old (cdr l))))
         (cons (car l) (insertL* new old (cdr l)))))
    (else (cons (insertL* new old (car l))
                (insertL* new old (cdr l))))))

(define (subst* new old l)
   (cond
    ((null? l) '())
    ((atom? (car l))
     (if (eq? (car l) old)
         (cons new (subst* new old (cdr l)))
         (cons (car l) (subst* new old (cdr l)))))
    (else (cons (subst* new old (car l))
                (subst* new old (cdr l))))))

(define (occur* a l)
   (cond
    ((null? l) 0)
    ((atom? (car l))
     (if (eq? (car l) a)
         (add1 (occur* a (cdr l)))
         (occur* a (cdr l))))
    (else (+ (occur* a (car l))
             (occur* a (cdr l))))))

(define (member* a l)
   (cond
    ((null? l) #f)
    ((atom? (car l))
     (or (eq? (car l) a)        
         (member* a (cdr l))))
    (else (or (member* a (car l))
              (member* a (cdr l))))))

(define (leftmost l)
  (if (atom? (car l))
      (car l)
      (leftmost (car l))))

(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l1)) #f)    
    (else
     (and (equal? (car 11) (car 12))
          (eqlist? (cdr 11) (cdr 12))))))

(define (equal? s1 s2)
  (cond 
    ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist? s1 s2))))
 
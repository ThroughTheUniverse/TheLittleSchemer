#lang racket

(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x) ))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (atom? (car l))
                 (lat? (cdr l)))))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old
                                 (cons new (cdr lat))))
      (else (cons (car lat)
                  (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat)
                  (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? o1 (car lat))
           (eq? o2 (car lat))) (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old
                                 (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new
                                 (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat)
                  (multisubst new old (cdr lat)))))))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (o- (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup)
                (addtup (cdr tup)))))))

(define *
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n)
               (sub1 m))))))

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n)
               (sub1 m))))))

(define =
  (lambda (n m)
    (cond
      ((zero? n) (zero? m))
      ((zero? m) #f)
      (else (= (sub1 n) (sub1 m))))))

(define expt
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (expt n (sub1 m)))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((= n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((= n 1) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat)
                  (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat)
                                 (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)
            (number? a2)) (= a1 a2))
      ((number? a1) #f)
      ((number? a2) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? a (car l)) (rember* a (cdr l)))
                         (else (cons (car l)
                                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? old (car l)) (cons old
                                                  (cons new
                                                        (insertR* new old (cdr l)))))
                         (else (cons (car l)
                                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond
                         ((eq? a (car l)) (add1 (occur* a (cdr l))))
                         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l))
                (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
                         (else (cons (car l)
                                     (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? old (car l)) (cons new
                                                  (cons old
                                                        (insertL* new old (cdr l)))))
                         (else (cons (car l)
                                     (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? a (car l))
                           (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)
            (null? l2)) #t)
      ((null? l1) #f)
      ((null? l2) #f)
      ((and (atom? (car l1))
            (atom? (car l2))) (and (eqan? (car l1) (car l2))
                                   (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((atom? (car l2)) #f)
      (else (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1)
            (atom? s2)) (eqan? s1 s2))
      ((atom? s1) #f)
      ((atom? s2) #f)
      (else (eqlist? s1 s2)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+) (and (numbered? (car aexp))
                                      (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '*) (and (numbered? (car aexp))
                                      (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'expt) (and (numbered? (car aexp))
                                         (numbered? (car (cdr (cdr aexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define edd
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (edd n (zub1 m)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat)
                  (makeset (cdr lat)))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1)
                                       (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel))
                         (first (car rel)))
                  (revrel (cdr rel)))))))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (second (car rel))
                  (seconds (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons new l))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons old
                                   (cons new
                                         (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new l)
    (cons new l)))

(define seqR
  (lambda (new l)
    (cons (car l) (cons new (cdr l)))))

(define seqS
  (lambda (new l)
    (cons new (cdr l))))

(define seqrem
  (lambda (new l)
    (l)))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? old (car l)) (seq new l))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else expt))))

(define operator
  (lambda (nexp)
    (car nexp)))

;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      (else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp))
;                                                (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test? (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL) (cons new
                                  (cons oldL
                                        (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR
                                  (cons new
                                        (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR-co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL) (multiinsertLR-co new oldL oldR (cdr lat)
                                              (lambda (newlat L R)
                                                (col (cons new
                                                           (cons oldL
                                                                 newlat)) (add1 L) R))))
      ((eq? (car lat) oldR) (multiinsertLR-co new oldL oldR (cdr lat)
                                              (lambda (newlat L R)
                                                (col (cons oldR
                                                           (cons new
                                                                 newlat)) L (add1 R)))))
      (else (multiinsertLR-co new oldL oldR (cdr lat)
                              (lambda (newlat L R)
                                (col (cons (car lat) newlat) L R)))))))

(define col
  (lambda (newlat L R)
    (cons newlat (cons L (cons R '())))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((even? (car l)) (cons (car l)
                                                (evens-only* (cdr l))))
                         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))

(define evens-only-co
  (lambda (l col)
    (cond
      ((null? l) (col '() 0 1))
      ((atom? (car l)) (cond
                         ((even? (car l)) (evens-only-co (cdr l)
                                                         (lambda (newlat sum mul)
                                                           (col (cons (car l) newlat)
                                                                sum
                                                                (* (car l) mul)))))
                         (else (evens-only-co (cdr l)
                                              (lambda (newlat sum mul)
                                                (col newlat (+ (car l)) mul))))))
      (else (evens-only-co (car l) (lambda (newlat sum mul)
                                     (evens-only-co (cdr l)
                                                    (lambda (newlat1 sum1 mul1)
                                                      (col (cons newlat newlat1)
                                                           (+ sum sum1)
                                                           (* mul mul1))))))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (length* (first pora)) 2)
               (length* (second pora)))))))

(define eternity
  (lambda (x)
    (eternity x)))

(define new-entry build)

(define lookup-in-entry 
  (lambda (name entry entry-f) 
    (lookup-in-entry-help name 
                          (first entry) 
                          (second entry) 
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? values) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr values)
                                  entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name (cdr table) table-f)))))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (l)
    (cond
      ((atom? (car l)) (cond
                         ((eq? (car l) 'quote) *quote)
                         ((eq? (car l) 'lambda) *lambda)
                         ((eq? (car l) 'cond) *cond)
                         (else *application)))
      (else *application))))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (second e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define question-of first)

(define answer-of second)

(define else?
  (lambda (question)
    (cond
      ((atom? question) (eq? question 'else))
      (else #f))))

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define *cond
  (lambda (e table)
    (evcon (cdr e) table)))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else (cons (meaning (car args) table)
                  (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
           (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? 'primitive (car l))))

(define non-primitive?
  (lambda (l)
    (eq? 'non-primitive (car l))))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      (else (apply-closure (second fun) vals)))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (first vals) (second vals)))
      ((eq? name 'car) (car (first vals)))
      ((eq? name 'cdr) (cdr (first vals)))
      ((eq? name 'null?) (null? (first vals)))
      ((eq? name 'eq?) (eq? (first vals) (second vals)))
      ((eq? name 'atom?) (:atom? (first vals)))
      ((eq? name 'zero?) (zero? (first vals)))
      ((eq? name 'add1) (add1 (first vals)))
      ((eq? name 'sub1) (sub1 (first vals)))
      ((eq? name 'number?) (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure)
                                      vals)
                           (table-of closure)))))
(apply-closure '((((u v w) 
                   (1 2 3)) 
                  ((x y z) 
                   (4 5 6))) 
                 (x y) 
                 (cons z x)) '((a b e) (d e f)))
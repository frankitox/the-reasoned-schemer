(load "mk.scm")
(load "mkextraforms.scm")
(load "07-a-bit-too-much.scm")

(define bound-*o
  (lambda (q p n m)
    (conde
      ((nullo q) (pairo p))
      (else
        (fresh (x y z)
          (cdro q x)
          (cdro p y)
          (condi
            ((nullo n)
             (cdro m z)
             (bound-*o x y z ()))
            (else
              (cdro n z)
              (bound-*o x y z m))))))))

(define odd-*o
  (lambda (x n m p)
    (fresh (q)
      (bound-*o q p n m)
      (*o x m q)
      (+o (cons 0 q) m p))))

(define *o
  (lambda (n m p)
    (condi
      ((== () n) (== () p))
      ((poso n) (== () m) (== () p))
      ((== '(1) n) (poso m) (== m p))
      ((>1o n) (== '(1) m) (== n p))
      ((fresh (x z)
         (== (cons 0 x) n) (poso x)
         (== (cons 0 z) p) (poso z)
         (>1o m)
         (*o x m z)))
      ((fresh (x y)
         (== (cons 1 x) n) (poso x)
         (== (cons 0 y) m) (poso y)
         (*o m n p)))
      ((fresh (x y)
         (== (cons 1 x) n) (poso x)
         (== (cons 1 y) m) (poso y)
         (odd-*o x n m p))))))

(run 2 (t)
  (fresh (n m)
    (*o n m (list 1))
    (== (list n m) t)))
;; (((1) (1)))

(run* (p)
  (*o (list 1 1 1) (list 1 1 1 1 1 1) p))
;; ((1 0 0 1 1 1 0 1 1))

(define =lo
  (lambda (n m)
    (conde
      ((== () n) (== () m))
      ((== '(1) n) (== '(1) m))
      (else
        (fresh (a b)
          (cdro n a) (poso a)
          (cdro m b) (poso b)
          (=lo a b))))))

(run* (t)
  (fresh (w x y)
    (=lo (cons 1 (cons w (cons x y)))
         (list 0 1 1 0 1))
    (== (list w x y) t)))
;; ((_.0 _.1 (_.2 ⚫ 1)))

(run* (b) (=lo '(1) (list b)))
;; (1)

(run* (n)
  (=lo (cons 1 (cons 0 (cons 1 n)))
       (list 0 1 1 0 1)))
;; ((_.1 ⚫ 1))

(run 5 (t)
  (fresh (y z)
    (=lo (cons 1 y) (cons 1 z))
    (== (list y z) t)))
;; ((() ())
;;  ((1) (1))
;;  ((_.0 ⚫ 1) (_.1 ⚫ 1))
;;  ((_.0 _.1 ⚫ 1) (_.2 _.3 ⚫ 1))
;;  ((_.0 _.1 _.2 ⚫ 1) (_.3 _.4 _.5 ⚫ 1)))

(run 5 (t)
  (fresh (y z)
    (=lo (cons 1 y) (cons 0 z))
    (== (list y z) t)))
;; (((1) (1))
;;  ((_.0 ⚫ 1) (_.1 ⚫ 1))
;;  ((_.0 _.1 ⚫ 1) (_.2 _.3 ⚫ 1))
;;  ((_.0 _.1 _.2 ⚫ 1) (_.3 _.4 _.5 ⚫ 1))
;;  ((_.0 _.1 _.2 _.3 ⚫ 1) (_.4 _.5 _.6 _.7 ⚫ 1)))

(define <lo
  (lambda (n m)
    (conde
      ((== n ()) (== m '(1)))
      ((== n '(1)) (>1o m))
      (else
        (fresh (x y)
          (cdro n x) (poso x)
          (cdro m y) (poso y)
          (<lo x y))))))

(run 8 (t)
  (fresh (y z)
    (<lo (cons 1 y)
         (cons 0 (cons 1 (cons 1 (cons 0 (cons 1 z))))))
    (== (list y z) t)))
;; ((() _.0)
;;  ((1) _.0)
;;  ((_.0 ⚫ 1) (_.1))
;;  ((_.0 _.1 ⚫ 1) (_.2))
;;  ((_.0 _.1 _.2 ⚫ 1) (_.3 ⚫ _.4))
;;  ((_.0 _.1 _.2 _.3 ⚫ 1) (_.4 _.5 ⚫ _.6))
;;  ((_.0 _.1 _.2 _.3 _.4 ⚫ 1) (_.5 _.6 _.7 ⚫ _.8))
;;  ((_.0 _.1 _.2 _.3 _.4 _.5 ⚫ 1) (_.6 _.7 _.8 _.9 ⚫ _.10)))

;; It has no value:
; (run 1 (n)
;   (<lo n n))

(define <=lo
  (lambda (n m)
    (conde
      ((=lo n m))
      ((<lo n m)))))

(run 8 (t)
  (fresh (n m)
    (<=lo n m)
    (== (list n m) t)))
;; Gets just =lo results.

(run 1 (t)
  (fresh (n m)
    (<=lo n m)
    (*o n (list 0 1) m)
    (== (list n m) t)))
;; (() ())

(define <=lo
  (lambda (n m)
    (condi
      ((=lo n m))
      ((<lo n m)))))

;; After a couple of boring exercises...

(define <o
  (lambda (n m)
    (condi
      ((<lo n m))
      ((=lo n m)
       (fresh (p)
         (poso p)
         (+o n p m))))))

(define <=o
  (lambda (n m)
    (condi
      ((== n m))
      ((<o n m)))))

(run* (q)
  (<o (list 1 0 1) (list 1 1 1))
  (== #t q))
;; (#t)

(run* (q)
  (<o (list 1 1 1) (list 1 0 1))
  (== #t q))
;; ()

(run* (q)
  (<o (list 1 0 1) (list 1 0 1))
  (== #t q))
;; ()

(run 6 (n)
  (<o n (list 1 0 1)))
;; (() (1) (0 1) (1 1) (0 0 1))

(run 6 (m)
  (<o (list 1 0 1) m))
;; ((_.0 _.1 _.2 _.3 ⚫ _.4)
;;  (0 1 1)
;;  (1 1 1))

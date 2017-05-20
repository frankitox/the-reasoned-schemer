(load "mk.scm")
(load "mkextraforms.scm")
(load "04-only-members.scm")

(define S succeed)
(define U fail)

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      (else
       (fresh (a d res)
        (caro l a)
        (cdro l d)
        (appendo d s res)
        (conso a res out))))))

(run* (x)
  (appendo
    '(cake)
    '(tastes yummy)
    x)) ;; '((cake tastes yummy))

(run* (x)
  (fresh (y)
    (appendo
      (list 'cake 'with 'ice y)
      (list 'tastes 'yummy)
      x)))
;; '((cake with ice _.0 tastes yummy))

(run* (x)
  (fresh (y)
    (appendo
      '(cake with ice cream)
      y
      x)))
;; '((cake with ice cream . _.0))

(run 1 (x)
  (fresh (y)
    (appendo
      (append '(cake with) (cons 'ice y))
      (list 'd 't)
      x)))
;; '((cake with ice d t))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      (else
       (fresh (a d res)
        (conso a d l)
        (appendo d s res)
        (conso a res out))))))

(run 5 (x)
  (fresh (y)
    (appendo
      (append '(cake with) (cons 'ice y))
      (list 'd 't)
      x)))
;; ((cake with ice d t)
;;  (cake with ice _.0 d t)
;;  (cake with ice _.0 _.1 d t)
;;  (cake with ice _.0 _.1 _.2 d t)
;;  (cake with ice _.0 _.1 _.2 _.3 d t))

(run 5 (y)
  (fresh (x)
    (appendo
      (append '(cake with) (cons 'ice y))
      (list 'd 't)
      x)))
;; (()
;;  (_.0 . ())
;;  (_.0 _.1 . ())
;;  (_.0 _.1 _.2 . ())
;;  (_.0 _.1 _.2 _.3 . ()))

(run 5 (x)
  (fresh (y)
    (appendo
      (append '(cake with) (cons 'ice y))
      (cons 'd (cons 't y))
      x)))
;; ((cake with ice d t)
;;  (cake with ice _.0 d t _.0)
;;  (cake with ice _.0 _.1 d t _.0 _.1)
;;  (cake with ice _.0 _.1 _.2 d t _.0 _.1 _.2)
;;  (cake with ice _.0 _.1 _.2 _.3 d t _.0 _.1 _.2 _.3))

(run* (x)
  (fresh (z)
    (appendo
      '(cake with ice cream)
      (cons 'd (cons 't z))
      x)))
;; '((cake with ice cream d t . _.0))

(run 6 (x)
  (fresh (y)
    (appendo x y '(cake with ice d t))))
;; (()
;;  (cake)
;;  (cake with)
;;  (cake with ice)
;;  (cake with ice d)
;;  (cake with ice d t))

(run 6 (y)
  (fresh (x)
    (appendo x y '(cake with ice d t))))
;; ((cake with ice d t)
;;  (with ice d t)
;;  (ice d t)
;;  (d t)
;;  (t)
;;  ())

(run 6 (r)
  (fresh (x y)
    (appendo x y '(cake with ice d t))
    (== (list x y) r)))
;; (() (cake with ice d t)
;;  (cake) (with ice d t)
;;  (cake with) (ice d t)
;;  (cake with ice) (d t)
;;  (cake with ice d) (t)
;;  (cake with ice d t) ())

;; This one never ends:
; (run 7 (r)
;   (fresh (x y)
;     (appendo x y '(cake with ice d t))
;     (== (list x y) r)))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      (else
       (fresh (a d res)
        (conso a d l)
        (conso a res out)
        (appendo d s res))))))

(run 7 (x)
  (fresh (y z)
    (appendo x y z)))
;; (()
;;  (_.0)
;;  (_.0 _.1)
;;  (_.0 _.1 _.2)
;;  (_.0 _.1 _.2 _.3)
;;  (_.0 _.1 _.2 _.3 _.4)
;;  (_.0 _.1 _.2 _.3 _.4 _.5))

(run 7 (y)
  (fresh (x z)
    (appendo x y z)))
;; (_.0 _.0 _.0 _.0 _.0 _.0 _.0)

(run 7 (z)
  (fresh (x y)
    (appendo x y z)))
;; ((_.0)
;;  (_.0 . _.1)
;;  (_.0 _.1 . _.2)
;;  (_.0 _.1 _.2 . _.3)
;;  (_.0 _.1 _.2 _.3 . _.4)
;;  (_.0 _.1 _.2 _.3 _.4 . _.5))
;;  (_.0 _.1 _.2 _.3 _.4 . _.5 _.6))

(run 7 (r)
  (fresh (x y z)
    (appendo x y z)
    (== (list x y z) r)))
;; (() _.0 _.0)
;; ((_.0) _.1 (_.0 . _.1))
;; ((_.0 _.1) _.2 (_.0 _.1 . _.2))
;; ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
;; ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 . _.3 _.4))
;; ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 . _.3 _.4 _.5))
;; ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 . _.3 _.4 _.5 _.6))

(define swappendo
  (lambda (l s out)
    (conde
      (S
        (fresh (a d res)
          (conso a d l)
          (conso a res out)
          (appendo d s res)))
      (else
        (nullo l)
        (== s out)))))

;; This one never ends.
; (run 1 (z)
;   (fresh (x y)
;     (swappendo x y z)))

(define unwrapo
  (lambda (x out)
    (conde
      ((pairo x)
       (fresh (a)
         (caro x a)
         (unwrapo a out)))
      (else
        (== x out)))))

(run* (x)
  (unwrapo '(((pizza))) x))
;; '(pizza (pizza) ((pizza)) (((pizza))))

;; It never ends.
; (run 1 (x)
;   (unwrapo x 'pizza))

;; It never ends.
; (run 1 (x)
;   (unwrapo '((x)) 'pizza))

(define unwrapo
  (lambda (x out)
    (conde
      (S
        (== x out))
      (else
        (fresh (a)
          (caro x a)
          (unwrapo a out))))))

(run 5 (x)
  (unwrapo x 'pizza))
;; (pizza
;;  (pizza . _.0)
;;  ((pizza . _.0) . _.1)
;;  (((pizza . _.0) . _.1) . _.2)
;;  ((((pizza . _.0) . _.1) . _.2) . _.3))

(run 5 (x)
  (unwrapo x '((pizza))))
;; (((pizza))
;;  (((pizza)) . _.0)
;;  ((((pizza)) . _.0) . _.1)
;;  (((((pizza)) . _.0) . _.1) . _.2)
;;  ((((((pizza)) . _.0) . _.1) . _.2) . _.3))

(run 5 (x)
  (unwrapo (list (list x)) 'pizza))
;; (pizza
;;  (pizza . _.0)
;;  ((pizza . _.0) . _.1)
;;  (((pizza . _.0) . _.1) . _.2)
;;  ((((pizza . _.0) . _.1) . _.2) . _.3))

(define flatteno
  (lambda (s out)
    (conde
      ((nullo s) (== s out))
      ((pairo s)
       (fresh (a d res-a res-d)
         (conso a d s)
         (flatteno a res-a)
         (flatteno d res-d)
         (appendo res-a res-d out)))
      (else
        (conso s () out)))))

(run 1 (x)
  (flatteno '((a b) c) x))
;; ((a b c))

(run 1 (x)
  (flatteno '((a b) c) x))
;; ((a b c))

(run* (x)
  (flatteno '(a) x))
;; ((a) (a ()) ((a)))

(run* (x)
  (flatteno '((a)) x))
;; Ahhh, little road to madness:
;; ((a)
;;  (a ())
;;  (a ())
;;  (a () ())
;;  ((a))
;;  ((a) ())
;;  (((a))))

(run* (x)
  (flatteno '(((a))) x))
;; ((a)
;;  (a ())
;;  (a ())
;;  (a () ())
;;  (a ())
;;  (a () ())
;;  (a () ())
;;  (a () () ())
;;  ((a))
;;  ((a) ())
;;  ((a) ())
;;  ((a) () ())
;;  (((a))))
;;  (((a))) ())
;;  ((((a)))))

(run* (x)
  (flatteno '((a b) c) x))

;; It never ends:
; (run* (x)
;   (flatteno x '(a b c)))

(define flattenrevo
  (lambda (s out)
    (conde
      (S (conso s () out))
      ((nullo s) (== s out))
      (else
       (fresh (a d res-a res-d)
         (conso a d s)
         (flattenrevo a res-a)
         (flattenrevo d res-d)
         (appendo res-a res-d out))))))

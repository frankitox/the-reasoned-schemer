(load "mk.scm")
(load "mkextraforms.scm")
(load "02-teaching-old-toys-new-tricks.scm")

(define S succeed)
(define U fail)

(define listo
  (lambda (l)
    (conde
      ((nullo l) S)
      ((pairo l) (fresh (x)
                   (cdro l x)
                   (listo x)))
      (else U))))

(run* (x)
  (listo (list 'a 'b x 'd))) ; (_.0)

(run 1 (x)
  (listo (cons 'a (cons 'b (cons 'c x)))))

(define lolo
  (lambda (l)
    (conde
      ((nullo l) S)
      ((fresh (a)
         (caro l a)
         (listo a))
       (fresh (r)
         (cdro l r)
         (lolo r)))
      (else U))))

(run 3 (l)
  (lolo l))

(define twinso
  (lambda (s)
    (fresh (x xs)
      (conso x xs s)
      (conso x () xs))))

(run* (q)
  (twinso '(tofu tofu))
  (== #t q)) ; (#t)

(run* (z)
  (twinso (list z 'tofu))) ; '(tofu)

; (define twinso
;   (lambda (s)
;     (fresh (x)
;       (== (list x x) s))))

(define loto
  (lambda (l)
    (conde
      ((nullo l) S)
      ((fresh (x)
         (caro l x)
         (twinso x))
       (fresh (xs)
         (cdro l xs)
         (loto xs)))
      (else U))))

(run 5 (z)
  (loto (cons '(g g) z)))
; (()
;  ((_.0 _.0))
;  ((_.0 _.0) (_.1 _.1)))
;  ((_.0 _.0) (_.1 _.1) (_.2 _.2))
;  ((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3)))

(run 5 (r)
  (fresh (w x y z)
    (loto
      (cons '(g g)
        (cons (list 'e w)
          (cons (list x y) z))))
    (== (list w (list x y) z) r)))
; ('e (_.0 _.0) ())
; ('e (_.0 _.0) ((_.1 _.1)))
; ('e (_.0 _.0) ((_.1 _.1) (_.2 _.2)))
; ('e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3)))
; ('e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3) (_.4 _.4)))

(run 3 (out)
  (fresh (w x y z)
    (== (cons '(g g)
          (cons (list 'e w)
            (cons (list x y) z))) out)
    (loto out)))
; ((('g 'g) ('e 'e) (_.0 _.0))
;  (('g 'g) ('e 'e) (_.0 _.0) (_.1 _.1))
;  (('g 'g) ('e 'e) (_.0 _.0) (_.1 _.1) (_.2 _.2))

(define listofo
  (lambda (predo l)
    (conde
      ((nullo l) S)
      ((fresh (x)
         (caro l x)
         (predo x)
       (fresh (xs)
         (cdro l xs)
         (listofo predo xs))))
      (else U))))

(run 3 (out)
  (fresh (w y x z)
    (== (cons '(g g)
          (cons (list 'e w)
            (cons (list x y) z))) out)
    (listofo twinso out)))
; Same as the previous example.

(define loto
  (lambda (l)
    (listofo twinso l)))

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define membero
  (lambda (x l)
    (conde
      ((eq-caro l x) S)
      (else (fresh (ll)
              (cdro l ll)
              (membero x ll))))))

(run* (q)
  (membero 'olive '(virgin olive oil))
  (== #t q)) ; (#t)

(run 1 (y)
  (membero y '(hummus with pita)))
; '(hummus)

(run* (y)
  (membero y ())) ; ()

(run* (y)
  (membero y '(hummus with pita)))
; '(hummus with pita)

(define identity
  (lambda (l)
    (run* (y)
      (membero y l))))

(run* (x)
  (membero 'e (list 'pasta x 'fagioli)))
; '(e)

(run 1 (x)
  (membero 'e (list 'pasta 'e x 'fagioli)))
; (_.0)

(run 1 (x)
  (membero 'e (list 'pasta x 'e 'fagioli)))
; '(e)

(run* (r)
  (fresh (x y)
    (membero 'e (list 'pasta x 'fagioli y))
    (== (list x y) r)))
; (('e _.0) (_.0 'e))

(run 1 (l)
  (membero 'tofu l))
; ((cons 'tofu _.0))

; (run* (l)
;   (membero 'tofu l))
; runs indefinitely.

(run 5 (l)
  (membero 'tofu l))
; (('tofu . _.0)
;  (_.0 'tofu . _.1)
;  (_.0 _.1 'tofu . _.2)
;  (_.0 _.1 _.2 'tofu . _.3)
;  (_.0 _.1 _.2 _.3 'tofu . _.4))

(define pmembero
  (lambda (x l)
    (conde
      ((nullo l) U)
      ((eq-caro l x) (cdro l ()))
      (else
        (fresh (d)
          (cdro l d)
          (pmembero x d))))))

(run 5 (l)
  (pmembero 'tofu l))
; (('tofu)
;  (_.0 'tofu)
;  (_.0 _.1 'tofu)
;  (_.0 _.1 _.2 'tofu)
;  (_.0 _.1 _.2 _.3 'tofu))

(run* (q)
  (pmembero 'tofu (list 'a 'b 'tofu 'd 'tofu))
  (== #t q))
; (#t)

(define pmembero
  (lambda (x l)
    (conde
      ((eq-caro l x) (cdro l ()))
      ((eq-caro l x) S)
      (else
        (fresh (d)
          (cdro l d)
          (pmembero x d))))))

(run* (q)
  (pmembero 'tofu (list 'a 'b 'tofu 'd 'tofu))
  (== #t q))
; (#t #t #t)

(define pmembero
  (lambda (x l)
    (conde
      ((eq-caro l x) (cdro l ()))
      ((eq-caro l x)
       (fresh (a d)
         (cdro l (cons a d))))
      (else
        (fresh (d)
          (cdro l d)
          (pmembero x d))))))

(run 12 (l)
  (pmembero 'tofu l))
; (('tofu)
;  ('tofu _.0 . _.1)
;  (_.0 'tofu)
;  (_.0 'tofu _.1 . _.2)
;  (_.0 _.1 'tofu)
;  (_.0 _.1 'tofu _.2 . _.3)
;  and so on.. )

(define first-value
  (lambda (l)
    (run 1 (y)
      (membero y l))))
; Quite similar to car, but safer.

(define memberrevo
  (lambda (x l)
    (conde
      ((nullo l) U)
      (S
        (fresh (d)
          (cdro l d)
          (memberrevo x d)))
      (else (eq-caro l x)))))

(run* (x)
  (memberrevo x '(pasta e fagioli)))
; '(fagioli e pasta)

(define reverse-list
  (lambda (l)
    (run* (e)
      (memberrevo e l))))

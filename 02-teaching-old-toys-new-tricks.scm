(load "mk.scm")
(load "mkextraforms.scm")
(load "01-playthings.scm")

(define S succeed)
(define U fail)

(let ((x (lambda (a) a))
      (y 'c))
  (x y)) ; 'c

(run* (r)
  (fresh (y x)
    (== (list x y) r)))
; ((_.0 _.1))

(run* (r)
  (fresh (v w)
    (== (let ((x v) (y w)) (list x y)) r)))
; ((_.0 _.1))

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(run* (r)
  (caro '(a c o r n) r)) ; '(a)

(run* (r)
  (fresh (x y)
    (caro (list r y) x)
      (== 'pear x))) ; '(pear)

(run* (q)
  (caro '(a c o r n) 'a)
  (== #t q)) ; (#t)
; Note that caro succeeds.

; I thought about defining `caro` like this:
;   (define caro
;     (lambda (p a)
;       (== (car p) a)))
; But, assumes `p` is a list. And this won't work:
;   (display (run* (q)
;     (fresh (x)
;       (caro x q))))

(run* (r)
  (fresh (x y)
    (caro '(grape raisin pear) x)
    (caro '((a) (b) (c)) y)
    (== (cons x y) r)))
; '((grape a))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(run* (r)
  (fresh (v)
    (cdro '(a c o r n) v)
    (caro v r))) ; '(c)

(run* (r)
  (fresh (x y)
    (cdro '(grape raisin pear) x)
    (caro '((a) (b) (c)) y)
    (== (cons x y) r))) ; '(((grape raisin) a))

(run* (q)
  (cdro '(a c o r n) '(c o r n))
  (== #t q)) ; (#t)

(run* (x)
  (cdro '(c o r n)
         (list x 'r 'n))) ; '(o)

(run* (l)
  (fresh (x)
    (cdro l '(c o r n))
    (caro l x)
    (== 'a x))) ; '((a c o r n))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(run* (l)
  (conso '(a b c) '(d e) l))
; '(((a b c) d e))

(run* (x)
  (conso x '(a b c) '(d a b c)))
; '(d)

(run* (r)
  (fresh (x y z)
    (== (list 'e 'a 'd x) r)
    (conso y (list 'a z 'c) r)))
; '((e a d c))

(run* (x)
  (conso x (list 'a x 'c) (list 'd 'a x 'c)))
; '(d)

(run* (l)
  (fresh (x)
    (== (list 'd 'a x 'c) l)
    (conso x (list 'a x 'c) l))) ; '((d a d c))

(run* (l)
  (fresh (x)
    (== (list 'd 'a x 'c) l)
    (conso x (list 'a x 'c) l)))
; '((d a d c))

(run* (l)
  (fresh (x)
    (conso x (list 'a x 'c) l)
    (== (list 'd 'a x 'c) l)))
; '((d a d c))

(run* (l)
  (fresh (d x y w s)
    (conso w (list 'a 'n 's) s)
    (cdro l s)
    (caro l x)
    (== 'b x)
    (cdro l d)
    (caro d y)
    (== 'e y)))
; '((b e a n s))

(define nullo
  (lambda (p)
    (== () p)))

(run* (q)
  (nullo '(grape raisin pear))
  (== #t q)) ; ()

(run* (q)
  (nullo ())
  (== #t q)) ; (#t)

(run* (x)
  (nullo x)) ; (())

(define eqo
  (lambda (a b)
    (== a b)))

(run* (r)
  (fresh (x y)
    (== (cons x (cons y 'salad)) r)))
; ((_.0 _.1 'salad))

(define pairo
  (lambda (p)
    (fresh (x y)
      (conso x y p))))

; Other definition simply using `pair?`
; (define pairo
;   (lambda (p)
;     (== #t (pair? p))))

(run* (q)
  (pairo (cons q q))
  (== #t q)) ; (#t)

(run* (q)
  (pairo ())
  (== #t q)) ; ()

(run* (x)
  (pairo x)) ; ((_.0 _.1))

(run* (r)
  (pairo (cons r 'pear))) ; '(_.0)

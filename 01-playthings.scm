(load "mk.scm")
(load "mkextraforms.scm")

(define S succeed)
(define U fail)

; #s it's a goal that succeeds.
; #u it's a goal that fails.

(run* (q) U) ; ()

(run* (q) (== #t q)) ; (#t)
; `==` is commutative.

(run* (q)
  U
  (== #t q)) ; ()

(run* (q)
  S
  (== #t q)) ; (#t)

(run* (q)
  S
  (== 'corn q)) ; '(corn)

(run* (r)
  U
  (== corn r)) ; ()

(run* (q)
  S
  (== #f q)) ; (#f)

; Does (== #f x) succeeds?
;   It depends on x.

; Does (let ((x #f)) (== #f x)) succeeds?
;   Yes, since #f is equal to #f.

(run* (x)
  (let ((x #f))
    (== #t x))) ; ()

(run* (q)
  (fresh (x)
    (== #t x)
    (== #t q))) ; (#t)

; (fresh (x ...) g ... ) binds fresh variables
; and succeeds if g ... succeed.

(run* (x) S) ; _.0
; `_.0` will represent a fresh variable.
; which is a variable with no association.
; note you can provoke variable shadowing.

(run* (x)
  (let ((x #f))
    (fresh (x)
      (== #t x)))) ; (_.0)

(run* (r)
  (fresh (x y)
    (== (cons x (cons y ())) r)))
; ((_.0 _.1))

(run* (s)
  (fresh (t u)
    (== (cons t (cons u ())) s)))
; ((_.0 _.1))

(run* (r)
  (fresh (x)
    (let ((y x))
      (fresh (x)
        (== (cons y (cons x (cons y ()))) r)))))
; ((_.0 _.1 _.0))

(run* (r)
  (fresh (x)
    (let ((y x))
      (fresh (x)
        (== (cons x (cons y (cons x ()))) r)))))
; ((_.0 _.1 _.0))

(run* (q)
  (== #f q)
  (== #t q)) ; ()

(run* (q)
  (== #f q)
  (== #f q)) ; (#f)

(run* (q)
  (let ((x q))
    (== #t x))) ; (#t)

(run* (r)
  (fresh (x)
    (== x r))) ; (_.0)
; When one variable is associated with
; another, we say they *co-refer* or *share*.

(run* (q)
  (fresh (x)
    (== #t x)
    (== x q))) ; (#t)

(run* (q)
  (fresh (x)
    (== x q)
    (== #t x))) ; (#t)

; Are q and x different variables? in:
(run* (q)
  (fresh (x)
    (== #t x)
    (== x q)))
; Every variable introduced by `fresh` or
; `run` is different from every other one
; introduced by `fresh` or `run`.

(cond
  (#f #t)
  (else #f)) ; #f (the one from the else)

(cond
  (#f S)
  (else U)) ; U

(conde
  (U S)
  (else U)) ; U

(conde
  (U U)
  (else S)) ; S

(conde
  (S S)
  (else U)) ; S

(run* (x)
  (conde
    ((== 'olive x) S)
    ((== 'oil x) S)
    (else U))) ; '(olive oil)
; To get more values from conde, pretend that
; the successful conde line has failed,
; refreshing all variables that got an
; association from that line.

(run 1 (x)
  (conde
    ((== 'olive x) S)
    ((== 'oil x) S)
    (else U))) ; '(olive)

(run* (x)
  (conde
    ((== 'virgin x) U)
    ((== 'olive x) S)
    (S S)
    ((== 'oil x) S)
    (else U))) ; '(olive _.0 oil)

(run 2 (x)
  (conde
    ((== 'extra x) S)
    ((== 'virgin x) U)
    ((== 'olive x) S)
    ((== 'oil x) S)
    (else U))) ; '(extra olive)

(run* (r)
  (fresh (x y)
    (== 'split x)
    (== 'pea y)
    (== (cons x (cons y ())) r)))
; '((split pea))

(run* (r)
  (fresh (x y)
    (conde
      ((== 'split x) (== 'pea y))
      ((== 'navy x) (== 'bean y))
      (else U))
    (== (cons x (cons y ())) r)))
; ((split pea) (navy bean))

(define teacup0
  (lambda (x)
    (conde
      ((== 'tea x) S)
      ((== 'cup x) S)
      (else U))))

(run* (x)
  (teacup0 x)) ; '(tea cup)

(run* (r)
  (fresh (x y)
    (conde
      ((teacup0 x) (== #t y) S)
      ((== #f x) (== #t y))
      (else U))
    (== (cons x (cons y ())) r)))
; ((tea #t) (cup #t) (#f #t))

(run* (r)
  (fresh (x y z)
    (conde
      ((== y x) (fresh (x) (== z x)))
      ((fresh (x) (== y x)) (== z x))
      (else U))
    (== (cons y (cons z ())) r)))
; ((_.0 _.1) (_.0 _.1))

(run* (r)
  (fresh (x y z)
    (conde
      ((== y x) (fresh (x) (== z x)))
      ((fresh (x) (== y x)) (== z x))
      (else U))
    (== #f x)
    (== (cons y (cons z ())) r)))
; ((#f _.0) (_.0 #f))

(run* (q)
  (let ((a (== #t q))
        (b (== #f q)))
    b))
; (#f)

(run* (q)
  (let ((a (== #t q))
        (b (fresh (x)
             (== x q)
             (== #f x)))
        (c (conde
             ((== #t q) S)
             (else (== #f q))))
    b)))
; (#f)

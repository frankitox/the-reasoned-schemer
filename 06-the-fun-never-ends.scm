(load "mk.scm")
(load "mkextraforms.scm")
(load "05-double-your-fun.scm")

(define S succeed)
(define U fail)

(define anyo
  (lambda (g)
    (conde
      (g S)
      (else
        (anyo g)))))

(define nevero (anyo U))

;; It has no value:
; (run 1 (q)
;   nevero
;   (== #t q))

(run 1 (q)
  U
  nevero) ;; ()

(define alwayso (anyo S))

(run 1 (q)
  alwayso
  (== #t q)) ;; (#t)

; It has no value.
;; (run* (q)
;;   alwayso
;;   (== #t q))

(run 5 (q)
  alwayso
  (== #t q))
;; (#t #t #t #t #t)

(run 5 (q)
  (== #t q)
  alwayso)
;; (#t #t #t #t #t)

(define salo
  (lambda (g)
    (conde
      (S S)
      (else g))))

(run 1 (q)
  (salo alwayso)
  (== #t q)) ;; (#t)

(run 1 (q)
  (salo nevero)
  (== #t q)) ;; (#t)

;; It has no value.
; (run* (q)
;   (salo nevero)
;   (== #t q))

;; It has no value.
; (run 1 (q)
;   (salo nevero)
;   U
;   (== #t q))

;; It has no value.
; (run 1 (q)
;   alwayso
;   U
;   (== #t q))

;; It has no value.
; (run 1 (q)
;   (conde
;     ((== #f q) alwayso)
;     (else (anyo (== #t q))))
;   (== #t q))

(run 1 (q)
  (condi
    ((== #f q) alwayso)
    (else (== #t q)))
  (== #t q))

;; it has no value since the
;; second condi line is out of values.
; (run 2 (q)
;   (condi
;     ((== #f q) alwayso)
;     (else (== #t q)))
;   (== #t q))

(run 5 (q)
  (condi
    ((== #f q) alwayso)
    (else (anyo (== #t q))))
  (== #t q)) ;; (#t #t #t #t #t)

;; condi behaves like conde, except that
;; interleaves the values!

(run 5 (r)
  (condi
    ((teacupo r) S)
    ((== #f r) S)
    (else U))) ;; ('tea #f 'cup)

(run 5 (q)
  (condi
    ((== #f q) alwayso)
    ((== #t q) alwayso)
    (else U))
  (== #t q)) ; (#t #t #t #t #t)

; if you use conde, then there's no value.

; (run 5 (q)
;   (condi
;     (alwayso S)
;     (else nevero))
;   (== #t q))
;; It has no value

; (run 1 (q)
;   (all
;     (conde
;       ((== #f q) S)
;       (else (== #t q)))
;     alwayso)
;   (== #t q)) ; It has no value.

; alli allows to 'switch' conde behavior.
(run 1 (q)
  (alli
    (conde
      ((== #f q) S)
      (else (== #t q)))
    alwayso)
  (== #t q)) ; (#t)

(run 5 (q)
  (alli
    (conde
      ((== #f q) S)
      (else (== #t q)))
    alwayso)
  (== #t q)) ; (#t #t #t #t #t)

(run 5 (q)
  (alli
    (conde
      (S (== #t q))
      (else (== #f q)))
    alwayso)
  (== #t q)) ; (#t #t #t #t #t)

(run 5 (q)
  (all
    (conde
      (S S)
      (else nevero))
    alwayso)
  (== #t q)) ; '(#t #t #t #t #t)
; if we replace all for alli, then
; it has no value because we enter
; in the nevero.


;; NOTE: understanding `all`.
(run* (r)
  (fresh (a b)
    (all
      (teacupo a)
      (conde
        ((== b 'blue) S)
        (else (== b 'red))))
    (== r (list a b))))

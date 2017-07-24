(load "mk.scm")
(load "mkextraforms.scm")
(load "06-the-fun-never-ends.scm")

(define bit-xoro
  (lambda (x y r)
    (conde
      ((== 0 x) (== 0 y) (== 0 r))
      ((== 0 x) (== 1 y) (== 1 r))
      ((== 1 x) (== 0 y) (== 1 r))
      ((== 1 x) (== 1 y) (== 0 r))
      (else U))))

(run* (r)
  (fresh (x y)
    (bit-xoro x y 0)
    (== (list x y) r)))

(run* (r)
  (fresh (x y)
    (bit-xoro x y 1)
    (== (list x y) r)))

(run* (s)
  (fresh (x y r)
    (bit-xoro x y r)
    (== s (list x y r))))

(define bit-ando
  (lambda (x y r)
    (conde
      ((== 0 x) (== 0 y) (== 0 r))
      ((== 0 x) (== 1 y) (== 0 r))
      ((== 1 x) (== 0 y) (== 0 r))
      ((== 1 x) (== 1 y) (== 1 r))
      (else U))))

(run* (r)
  (fresh (x y)
    (bit-ando x y 1)
    (== (list x y) r)))

(define half-addero
  (lambda (x y r c)
    (all
      (bit-xoro x y r)
      (bit-ando x y c))))

(run* (r)
  (half-addero 1 1 r 1))

(run* (s)
  (fresh (x y r c)
    (half-addero x y r c)
    (== (list x y r c) s)))

(define full-addero
  (lambda (b x y r c)
    (fresh (w xy wz)
      (half-addero x y w xy)
      (half-addero w b r wz)
      (bit-xoro xy wz c))))

(run* (s)
  (fresh (r c)
    (full-addero 0 1 1 r c)
    (== (list r c) s))) ; ((0 1))

(run* (s)
  (fresh (r c)
    (full-addero 1 1 1 r c)
    (== (list r c) s))) ; ((1 1))

(run* (s)
  (fresh (b x y r c)
    (full-addero b x y r c)
    (== (list b x y r c) s)))
;   b x y r c
; ((0 0 0 0 0)
;  (1 0 0 1 0)
;  (0 1 0 1 0)
;  (1 1 0 0 1)
;  (0 0 1 1 0)
;  (1 0 1 0 1)
;  (0 1 1 0 1)
;  (1 1 1 1 1))

(define build-num
  (lambda (n)
    (if (zero? n) ()
      (if (= n 1) '(1)
        (cons (remainder n 2)
              (build-num (quotient n 2)))))))

(define build-num
  (lambda (n)
    (cond
      ((zero? n) ())
      ((and (not (zero? n)) (even? n))
       (cons 0 (build-num (quotient n 2))))
      ((odd? n)
       (cons 1 (build-num (quotient n 2)))))))

(define poso
  (lambda (n)
    (fresh (a d)
      (== (cons a d) n))))

(run* (q)
  (poso (list 0 1 1))
  (== #t q)) ; (#t)

(run* (q)
  (poso (list 1))
  (== #t q)) ; (#t)

(run* (q)
  (poso ())
  (== #t q)) ; ()

(run* (r)
  (poso r)) ; ((_.0 . _.1))

(define >1o
  (lambda (n)
    (fresh (x y z)
      (== (cons x (cons y z)) n))))

(run* (q)
  (>1o (list 0 1 1))
  (== #t q)) ; (#t)

(run* (q)
  (>1o (list 0 1))
  (== #t q)) ; (#t)

(run* (q)
  (>1o (list 1))
  (== #t q)) ; ()

(run* (q)
  (>1o ())
  (== #t q)) ; ()

(run* (r)
  (>1o r)) ; ((_.0 _.1 . _.2))

; (run 3 (s)
;   (fresh (x y r)
;     (addero 0 x y r)
;     (== (list x y r) s)))

(define addero
  (lambda (d n m r)
    (condi
      ((== 0 d) (== () m) (== n r))
      ((== 0 d) (== () n) (== m r) (poso m))
      ((== 1 d) (== () m) (addero 0 n '(1) r))
      ((== 1 d) (== () n) (addero 0 '(1) m r) (poso m))
      ((== '(1) n) (== '(1) m) (fresh (a c)
                                 (== r (cons a c))
                                 (full-addero d 1 1 a c)))
      ((== '(1) n) (gen-addero d n m r))
      ((== '(1) m) (>1o n) (>1o r) (addero d '(1) n r))
      ((>1o n) (gen-addero d n m r)))))

(define gen-addero
  (lambda (d n m r)
    (fresh (a b c d e x y z)
      (== (cons a x) n)
      (== (cons b y) m) (poso y)
      (== (cons c z) r) (poso z)
      (alli
        (full-addero d a b c e)
        (addero e x y z)))))

(run* (s)
  (gen-addero 1 '(0 1 1) '(1 1) s))
;; (0 1 0 1)

(run* (s)
  (fresh (x y)
    (addero 0 x y (list 1 0 1))
    (== (list x y) s)))
;; ((1 0 1) ())
;; (() (1 0 1))
;; ((1) (0 0 1))
;; ((0 0 1) (1))
;; ((0 1) (1 0))
;; ((1 0) (0 1))

(define +o
  (lambda (x y z)
    (addero 0 x y z)))

(run* (s)
  (fresh (x y)
    (+o x y '(1 0 1))
    (== (list x y) s)))
;; Same as previous example.

(define -o
  (lambda (x y z)
    (+o z y x)))

(run* (q)
  (-o '(0 0 0 1) '(1 0 1) q)) ;; (1 1)

(run* (q)
  (-o '(0 1 1) '(0 1 1) q)) ;; ()

(run* (q)
  (-o '(0 1 1) '(0 0 0 1) q)) ;; ()

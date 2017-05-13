(load "mk.scm")
(load "mkextraforms.scm")
(load "03-seeing-old-friends-in-new-ways.scm")

(define S succeed)
(define U fail)

(define memo
  (lambda (x l out)
    (conde
      ((nullo l) U)
      ((eq-caro l x) (== l out))
      (else
       (fresh (ll)
        (cdro l ll)
        (memo x ll out))))))

(run 1 (out)
  (memo 'tofu '(a b tofu d tofu e) out))
;; '((tofu d tofu e))

(run 1 (out)
  (fresh (x)
    (memo 'tofu (list 'a 'b x 'd 'tofu 'e) out)))
;; '((tofu d tofu e))

(run* (r)
  (memo r
    '(a b tofu d tofu e)
    '(tofu d tofu e))) ;; '(tofu)

(run* (q)
  (memo 'tofu '(tofu e) '(tofu e))
  (== #t q)) ;; (#t)

(run* (q)
  (memo 'tofu '(tofu e) '(tofu))
  (== #t q)) ;; ()

(run* (x)
  (memo 'tofu '(tofu e) (list x 'e)))
;; '(tofu)

(run* (x)
  (memo 'tofu '(tofu e) (list 'peas x)))
;; ()

(run* (out)
  (fresh (x)
    (memo 'tofu (list 'a 'b x 'd 'tofu 'e) out)))
;; '(('tofu 'd 'tofu 'e) ('tofu 'e))

(run 12 (z)
  (fresh (u)
    (memo
      'tofu
      (append '(a b tofu d tofu)
               (cons 'e z))
      u)))
;; _.0
;; _.0
;; '(tofu . _.0)
;; '(_.0 tofu . _.1)
;; '(_.0 _.1 tofu . _.2)
;; and so on...

(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== () out))
      ((eq-caro x l) (cdro l out))
      ((fresh (res)
         (fresh (d)
           (cdro l d)
           (rembero x d res))
         (fresh (a)
           (caro l a)
           (conso a res out)))))))

; (define rembero
;   (lambda (x l out)
;     (conde
;       ((nullo l) (== () out))
;       ((eq-caro x l) (cdro l out))
;       ((fresh (a d res)
;          (conso a d l)
;          (rembero x d res)
;          (conso a res out))))))

(run 1 (out)
  (fresh (y)
    (rembero 'peas
             (list 'a 'b y 'd 'peas 'e) out)))
;; '(a b d peas e)

(display (run 1 (out)
  (fresh (y z)
    (rembero y (list 'a 'b y 'd z 'e) out))))
;; (('a 'b 'd _.0 'e) ('e))

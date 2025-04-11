;; Exercise 1.1
10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) ;; a
(define b (a + 1)) ;; 4
(+ a b (* a b)) ;; 19
(= a b) ;; f
(if (and (> b a) (< b (*a b)))
    b
    a) ;; b
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1)) ;; 16

;; Exercise 1.2
(/ (+ 5 4 (+ (- 2 (- 3 (+ 6 (/ 5 4))))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
(define (square x)
  (* x x))

(define (sum-square x y)
  (+ (square x) (square y)))

(define (square-max x y z)
  (cond ((and (< x y) (< x z)) (sum-square y z))
	((and (< y x) (< y z)) (sum-square x z))
	(else (sum-square x y))))

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)) ;; Suma a a el abs de b y programatimamente
;; devuelve el operando a usar siendo + o -

;; Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
;; Si LISP usas normal-order evaluation, esto no se romperia
;; Dado que y nunca se usa,
;; De todas formas, aunque LISP sea applicativo no se va a romper
;; Ya que el if es una special-form que solo evalua lo que necesita 

;; Exercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
(new-if (= 2 3) 0 5) ;; 5
(new-if (= 1 1) 0 5) ;; 0

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

;; En este caso, como LISP es applicativo este new-if si se va a romper
;; ya que siempre va a entrar en el caso recursivo para substituir

;; Exercise 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;; LINEAR RECURSIVE PROCESS
;; (+ 4 4)
;; (inc (+ 3 4))
;; (inc (inc (+ 2 4)))
;; (inc (inc (inc (+ 1 4))))
;; (inc (inc (inc (inc (+ 0 4)))))
;; (inc (inc (inc (inc 4))))
;; (inc (inc (inc 5)))
;; (inc (inc 6))
;; (inc 7)
;; 8


(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
;; LINEAR ITERATIVE PROCESS
;; (+ 4 4)
;; (+ 3 5)
;; (+ 2 6)
;; (+ 1 7)
;; (+ 0 8)
;; 8

;; Exercise 1.10: Ackermann

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

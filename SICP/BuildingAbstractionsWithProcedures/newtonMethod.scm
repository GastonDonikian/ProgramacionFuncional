;; Metodo de Newton
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.7
;; Si los numeros son muy chicos my DIFF puede no ser lo suficientemente grande
;; Por otro lado, si los numeros son muy grandes el cuadrado de esos numeros se puede romper
;; Hacerlo con un diff entre EPOCHs es otra alternativa
;; Aunque para numeros pequenos seguis teniendo un problema pero de menor categoria
;; No estoy muy seguro igual
(define (diff-sqrt-iter guess prev-guess x)
  (if (diff-good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x)
		 guess
		 x)))

(define (diff-good-enough? guess prev-guess)
  (< (/ guess prev-guess) 0.01)) ;; Una mejora de 1% es suficientemente poco (ponele)

;; Exercise 1.8
;; Solo hay que cambiar el improve...
(define (cubert-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

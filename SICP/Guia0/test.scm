-37 ; -37
(* 3 4) ; 12
(> 10 9.7) ; t

(- (if (> 3 4)
       7
       10)
   (/ 16 10)) ; 8.4

(* (- 25 10)
   (+ 6 3)) ; 24

+ ; Algun print de proc

(define double (lambda (x) (* 2 x))) ; Otro print de proc, double

double ; compound-procedure

(define c 4) ; c

c ; 4

(double c) ; 8

(double (double (+ c 5))) ; 36

(define times-2 double) ; times-2

(times-2 c) ; 8

(define d c) ; d

(= c d) ; t

(cond (
       (>= c 2) 
       d)
      ((= c (- d 5)) (+ c d))
      (else (abs (- c d)))) ; 4


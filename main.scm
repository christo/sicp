; some random scheme code while following sicp book 2nd ed

; test an expression and print it with evaluation and expected
; value assertion, to be used for documenting code and verifying
; its expected output 
(define (testex expq val)
  (let ((result (eval expq)))
    (printf "~a\n> ~a" expq result)
    (cond ((eq? result val) (display "\nok\n"))
          (else (printf "\nexpected ~a\n" val)))))

; from section 1.3.3
(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

(define (sqrt x)
  (fixed-point
    (lambda (y) (average (/ x y) y))
    1))

(define (average x y) (/ (+ x y) 2))

(testex '(sqrt 64) 8)

(define (sqrt2 x)
  (fixed-point
    (average-damp (lambda (y) (/ x y)))
    1))

(define average-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))

(define (squareit x) (* x x))

(testex '(squareit 10) 100)

(define (newton f guess)
  (define df (deriv f))
  (fixed-point
    (lambda (x) (- x (/ (f x) (df x))))
    guess))

(define (newton-sqrt x)
  (newton (lambda (y) (- x (squareit y)))
          1))

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x)
          dx)))))

(define dx 0.000001)


; section 2.1.1

;"george" will make the procedures make-rat, numer, denom

(define (+rat x y)
  (make-rat 
    (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
    (* (denom x) (denom y))))


(define (*rat x y)
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

; now george will implement the data parts

(define (make-rat n d)
  (let ((g (gcd n d)))
  (cons (/ n g)
        (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

; alonzo church hack to make cons, car and cdr out of lambdas
(define (ac-cons x y)
  (lambda (m) (m x y)))
(define (ac-car x)
  (x (lambda (a d) a)))
(define (ac-cdr x)
  (x (lambda (a d) d)))

(testex '(ac-car (ac-cons 35 47)) 35)

; cons car and cdr can be defined in terms of pick or dispatch
; let's try to make a triple and a corresponding first, second and third

(define (triple x y z)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          ((= m 2) z)
          (else (error "arg not 0, 1, 2 -- triple" m)))))

(define (first z) (z 0))
(define (second z) (z 1))
(define (third z) (z 2))

(testex '(second (triple "foo" "bar" "bing")) "bar")

; local redefinition, shadow bindings and environments
(define new-plus 
  (lambda ()
  (define (+ x y)
    (* x y))
  (+ 5 7)))

(testex '(new-plus) 35)



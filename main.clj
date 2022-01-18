#!/usr/bin/env clj -M 
(ns sicp.core)

(defn abs [x] (max x (- x)))

(defn testex [expq val]
  (let [result (eval expq)]
    (printf "%s\n> %s" expq result)
    (cond
      (= result val) (println "\nok")
      :else (printf "\nexpected %s\n" val))))

(defn average [x y] (/ (+ x y) 2))

(defn fixed-point
  [f start]
  (def tolerance 0.00001)
  (defn close-enuf? [u v]
    (< (abs (- u v )) tolerance))
  (defn iter [old new]
    (if (close-enuf? old new)
    new
    (iter new (f new))))
  (iter start (f start)))

(defn sqrt [x]
  (fixed-point (fn [y] (average (/ x y) y))
               1))

(testex (sqrt 64) 8)

(defn average-damp []
  (fn [f]
    (fn [x] (average (f x) x))))

(defn sqrt2 [x]
  (fixed-point
    (average-damp (fn [y] (/ x y)))
    1))

(defn squareit [x] (* x x))

(testex '(squareit 10) 100)

(def dx 0.000001)

(defn deriv []
  (fn [f]
    (fn [x]
      (/ (- (f (+ x dx))
            (f x)
          dx)))))


(defn newton [f guess]
  (def df (deriv f))
  (fixed-point
    (fn [x] (- x (/ (f x) (df x))))
    guess))

(defn newton-sqrt [x]
  (newton (fn [y] (- x (squareit y)))
          1))

; euclid's algorithm
(defn gcd [a b]
  (if (= b 0)
      a
      (gcd b (mod a b))))

; section 2.1.1

;"george" will make the procedures make-rat, numer, denom

(defn make-rat [n d]
  (let [g (gcd n d)]
  (cons (/ n g)
        (/ d g))))

; now george will implement the data parts

; define car and cdr for clojure
(defn car [x] (first x))
(defn cdr [x] (second x))

(defn numer [x] (car x))
(defn denom [x] (cdr x))

(defn +rat [x y]
  (make-rat 
    (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
    (* (denom x) (denom y))))


(defn *rat [x y]
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))


; alonzo church hack to make cons, car and cdr out of lambdas
(defn ac-cons [x y]
  (fn [m] (m x y)))
(defn ac-car [x]
  (x (fn [a d] a)))
(defn ac-cdr [x]
  (x (fn [a d] d)))

(testex '(ac-car (ac-cons 35 47)) 35)

; cons car and cdr can be defined in terms of pick or dispatch
; let's try to make a triple and a corresponding first, second and third

(defn triple [x y z]
  (fn [m]
    (cond (= m 0) x
          (= m 1) y
          (= m 2) z
          ; the following shouldn't happen here
          :else (.prinln *err* "arg not 0, 1, 2 -- triple" m))))

(defn my-first [z] (z 0))
(defn my-second [z] (z 1))
(defn my-third [z] (z 2))

(testex '(my-second (triple "foo" "bar" "bing")) "bar")

; local redefinition, shadow bindings and environments,
; special clojure way, note it doesn't work
(defn new-plus []
  (with-redefs 
    [+ (fn [x y] (* x y))]
    (+ 5 7)))

(testex '(new-plus) 35)



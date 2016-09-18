#lang racket

; returns the sum of the values of the coins, given the #
; of pennies, dimes, nickels, and quarters
(define (sum-coins p d n q)
  (+ (* p 1) (* d 10) (* n 5) (* q 25)))

; returns the area of a pipe, given the height, inner radius,
; and the thickness 
(define (area-pipe-s h ir th)
  (* 2 (+ (* 2 (* 3.14 (expt (+ th ir) 2))) (* 2 3.14 ir h))))

; returns the area of a circle given the radius r 
(define (area-circle r)
  (* (expt r 2) 3.14))

; return circumference of a circle with radius r 
(define (circumference r)
  (* 2 3.14 r))

; return the surface area of a pipe, given the height, inner radius
; and the thickness
(define (area-pipe h ir th)
  (* 2 (+ (* 2 (area-circle (+ ir th))) (* (circumference ir) h))))

; returns the state tax given a worker's gross wages 
(define (tax wage)
  (cond
    [(or (= wage 10000) (= wage 30000) (= wage 20000) (> wage 40000))
     "This is something i don't know how to calculate"]
    [(< wage 10000)
    0]
    [(< wage 20000)
     (* wage .05)]
    [(< wage 30000)
     (* wage .08)]
    [(< wage 40000)
     (* wage .13)]))

; returns the value of the determinant
(define (determinant a b c)
  (- (expt b 2) (* 4 a c)))

; returns a list of the two solutions to the quadratic
; formula given the coefficients a, b, c (if there is a defined solution)
(define (quadratic a b c)
  ; error checking 
  (cond
    [(equal? 0 a)
      "No solution"]
    [
     (let* ([d (sqrt (determinant a b c))]
         [minus (- b d)]
         [plus  (+ b d)])
       (list (/ minus (* 2 a)) (/ plus (* 2 a))))
    ]
   )
)
; returns true if any is a pairing
(define (atom? any)
  (pair? any)
)
; returns sum of values in the list lst
(define (sum lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst) (sum (rest lst)))]
  )
)
; returns length of the list lst 
(define (length-list lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (length-list (rest lst)))]
  )
)
; returns avg of the values in the list lst 
(define (average-list lst)
  (/ (sum lst) (length-list lst))
)

  
         
  

#lang racket

; returns true if its argument is not a pair and false if it is.
(define (atom? any)
  (not (pair? any)))

(printf "Atom? testcase: (atom? '())\n")
(atom? '())
(printf "\n")

; sums the values in the lst 
(define (sum lst)
  (cond [(empty? lst) 0]
        [else (+ (first lst) (sum (rest lst)))]))

; averages the values in lst
(define (average-list lst)
  (/ (sum lst) (length lst)))

(printf "Test case average-list: (average-list '(1 2 3 4 5))\n")
(average-list '(1 2 3 4 5))
(printf "\n")

; remove words from lst-any which do not start with char
(define (remove-words lst-any char)
  ; check if char is a character 
  (if (not (char? char)) (error "char isn't a character: char = " char)
  (cond
      [(empty? lst-any) '()]
      ; check if not alphabetic char
      [(not (char-alphabetic? char))
       (error "char is not an alphabetic character: char =" char)]
      [else
        (let ([f (first lst-any)])
          (cond
            [(equal? (string-ref f 0) char)
             (cons f (remove-words (rest lst-any) char))]
            [else
             (remove-words (rest lst-any) char)]))])))

(printf "Remove-words testcase: (remove-words #\\k '(zorro zero-mostel kate zed apple)\n")
(remove-words '("zorro" "zero-mostel" "kate" "zed" "apple") #\z)
(printf "\n")

; returns a list of all the suffixes of lst-any
(define (suffixes list-any)
  (cond
    [(empty? list-any)
     '()]
    [else
     (cons list-any (suffixes (rest list-any))) ]))

(printf "Suffixes testcase: (suffixes '(a b \"monkey\" d))\n")
(suffixes '(a b "monkey" d))
(printf "\n")

(define (flatten lstOf-lstOf-nums)
  (cond
   [(empty? lstOf-lstOf-nums) '()]
   [else
     (append (first lstOf-lstOf-nums) (flatten (rest lstOf-lstOf-nums)))]))

(printf "Flatten testcase: (flatten '((1 2) (3 4 5) (6)))\n")
(flatten '((1 2) (3 4 5) (6)))
(printf "\n")
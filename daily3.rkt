#lang racket
(require rackunit)
(displayln "Number 1")
(define (parity-map list-num)
  (map (lambda (num)
         (cond
            [(equal? (remainder num 2) 0) "even"]
            [else "odd"]))
     list-num))

(check-match (parity-map '(1 2 3 4 5)) '("odd" "even" "odd" "even" "odd"))
(parity-map '(1 2 3 4 5))
(println "-------------------")



(displayln "Number 2")
; returns true if str is the even literal "even", false otherwise 
(define (even-literal? str)
  (string=? "even" str))

; returns number of even literals ("even") in (parity-map list-num)
(define (even-parity-count list-num)
  (let* ([parity-list (parity-map list-num)])
     (length (filter even-literal? parity-list))))

(check-equal? (even-parity-count '(1 2 3 4 5)) 2)
(even-parity-count '(1 2 3 4 5))
(println "-------------------")



(displayln "Number 3")
; removes the words from list-str that do not start w/ ch



(define (r-w-helper list-ch list-str)
  (foldl (lambda (ch str result-list)
         (cond
           [(equal? ch (string-ref str 0)) (cons str result-list)]
               [else result-list]))
       '()
       list-ch
       list-str
       )
)

(define (r-w ch list-str)
  (r-w-helper (make-list (length list-str) ch) list-str))
  
 
(check-match (r-w #\a '("apple" "oranges")) '("apple"))
(r-w #\a '("apple" "oranges"))
(check-match (r-w #\r '("hello" "recursive" "scheme" "rdlof")) '("rdlof" "recursive"))
(r-w #\r '("hello" "recursive" "scheme" "rdlof"))
(println "-------------------")
; end number 3



(displayln "Number 4")
(define (average-list list-num)
  ; multiply by 1.0 to convert to decimal
  (* (/ (foldl + 0 list-num) (length list-num)) 1.0)) 

(check-equal? (average-list '(1 2 3 4)) 2.5)
(average-list '(1 2 3 4))
(println "-------------------")

; end Number 4



(displayln "Number 5")
(define (flatten list-list-nums)
  (foldr append '() list-list-nums))

(check-match (flatten '((1) (1 2) (1 2 3))) '(1 1 2 1 2 3))
(flatten '((1) (1 2) (1 2 3)))
(println "-------------------")
; end Number 5



(displayln "Number 6")

(define (count-the-matches sym list-sym)
  (cond [(empty? list-sym)
         0] ; ret val 
        [(equal? sym (first list-sym))
         (+ 1 (count-the-matches sym (rest list-sym)))] ; ret val
        [else (count-the-matches sym (rest list-sym))]))

; 3 1s in the list 
(check-equal? (count-the-matches 1 '(2 1 1 1)) 3)
(count-the-matches 1 '(2 1 1 1))
(println "-------------------")
; end number 6



(displayln "Number 7")

(define (ctm-helper list-sym1 list-sym2)
  (foldr (lambda (sym1 sym2 result)
           (cond [(equal? sym1 sym2) (+ 1 result)]
                 [else result]))
         0
         list-sym1
         list-sym2))
         
(define (count-the-matches-foldr sym list-sym)
  (ctm-helper (make-list (length list-sym) sym) list-sym))

; 3 twos in the list
(check-equal? (count-the-matches-foldr 2 '(2 2 2 1)) 3) 
(count-the-matches-foldr 2 '(2 2 2 1))
; 0 "" in the list
(check-equal? (count-the-matches-foldr "" '("1" "2" "3")) 0) 
(count-the-matches-foldr "" '("1" "2" "3"))
(println "-------------------")
; end number 7



(displayln "Number 8")

(define (check-list lst i)
  (cond [(empty? lst) '() ]
        [(list? (first lst))
         (cons (first lst) (check-list (rest lst) (add1 i)))]
        [else
         (cons (list (first lst)) (check-list (rest lst) (add1 i)))]))

(define (bucket-helper list-num)
  (foldl (lambda (num1 num2 rl)
           (cond
             [(equal? num2 "1") rl]
             [(and (eq? num1 num2) (empty? rl))
              (list (list num1 num2))]
             [(eq? num1 num2)
              (cond [(list? (last rl))
                     (list-set rl (- (length rl) 1) (append (last rl) (list num2)))]
                    [else
                     (list-set rl (- (length rl) 1) (cons (last rl) (list num2)))])]
             [else
              (append rl (list num2))]
             ))
         '()
         list-num
         (append (rest list-num) '("1"))))

  (define (bucket list-num)
    (check-list (bucket-helper list-num) 0))

(check-match (bucket '(1 1 2 3 3)) '((1 1) (2) (3 3)))
(bucket '(1 1 2 3 3))
(check-match (bucket '(1 1 1 3 3 3)) '((1 1 1) (3 3 3)))
(bucket '(1 1 1 3 3 3))

#lang racket
; Number 1
(define (parity-map list-num)
  (map (lambda (num)
         (cond
            [(equal? (remainder num 2) 0) "even"]
            [else "odd"]))
     list-num))

;(parity-map '(1 2 3 4 5))
; End number 1 

; Number 2
; returns true of str is the even literal "even", false otherwise 
(define (even-literal? str)
  (string=? "even" str))

(define (even-parity-count list-num)
  (let* ([parity-list (parity-map list-num)])
     (length (filter even-literal? parity-list))))

;(even-parity-count '(1 2 3 4 5))

; Number 3
; removes the words from list-str that do not start w/ ch



(define (r-w-helper list-ch list-str)
  (foldr (lambda (ch str result-list)
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
  
 

;(r-w #\a '("apple" "oranges"))
;(r-w #\r '("hello" "recursive" "scheme" "rdlof"))
; end number 3

; Number 4
(define (average-list list-num)
  (/ (foldl + 0 list-num) (length list-num)))

;(average-list '(1 2 3 4))

; end Number 4

;(define (flatten lstOf-lstOf-nums)
;  (cond
;   [(empty? lstOf-lstOf-nums) '()]
;  [else
;     (append (first lstOf-lstOf-nums) (flatten (rest lstOf-lstOf-nums)))]))

; Number 5
(define (flatten list-list-nums)
  (foldr append '() list-list-nums))

;(flatten '((1) (1 2) (1 2 3)))

; end Number 5

; Number 6

(define (count-the-matches sym list-sym)
  (cond [(empty? list-sym)
         0] ; ret val 
        [(equal? sym (first list-sym))
         (+ 1 (count-the-matches sym (rest list-sym)))] ; ret val
        [else (count-the-matches sym (rest list-sym))]))

;(count-the-matches 1 '(2 1 1 1))

; end number 6

; Number 7

(define (ctm-helper list-sym1 list-sym2)
  (foldr (lambda (sym1 sym2 result)
           (cond [(equal? sym1 sym2) (+ 1 result)]
                 [else result]))
         0
         list-sym1
         list-sym2))
         
(define (count-the-matches-foldr sym list-sym)
  (ctm-helper (make-list (length list-sym) sym) list-sym))

;(count-the-matches-foldr 2 '(2 2 2 1))
;(count-the-matches-foldr "" '("1" "2" "3"))

; end number 7

; Number 8


(define (bucket list-num)
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

(bucket '(1 1 2 3 3))
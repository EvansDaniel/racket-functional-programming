#lang plai-typed

; Number 1
(define-type family-tree
  [person
   (name : string)
   (birthyear : number)
   (eyecolor : symbol)
   (mother : family-tree)
   (father : family-tree)
  ]
  [unknown])

; define daves tree and my tree
(define me (person "Daniel Evans" 1996 'blue
                  (person "Lisa Virella" 1974 'blue (unknown) (unknown))
                  (person "Brian" 1970 'green (unknown) (unknown))))

(define daves-tree (person "Dave" 1977 'brown
                           (person "Ken" 1945 'brown (unknown) (unknown))
                           (person "Mary Ellen" 1946 'brown (unknown) (unknown))))

; Number 2
(define (count-persons person)
  (cond [(unknown? person) 0]
        [else (+ 1 (+ (count-persons (person-mother person)) (count-persons (person-father person))))]))

(test (count-persons me) 3)

; Number 3
(define (age person)
  (cond [(unknown? person) 0]
        [else (- 2016 (person-birthyear person))]))

(define (average-age-helper people)
  (cond [(unknown? people) 0]
        [else (+ (age people)
                 (+
                  (age (person-mother people))
                  (age (person-father people))))]))

(define (average-age people)
  (/ (average-age-helper people) (count-persons people)))

(test (average-age me) 36)

; Number 4

(define-type p 
  [point
   (x : number)
   (y : number)])

(define-type shape
  [circle
   (center : p)
   (radius : number)]
  [rect
   (top-left : p)
   (length : number)
   (width : number)]
  [square
   (top-left : p)
   (length : number)])

(define c (circle (point 10 20) 2))
(define s (square (point 25 0) 2))
(define r (rect (point 30 15) 2 10))

; Number 5
(define (area s)
  (type-case shape s
    [circle (center r) (* (* r r) 3.141592)]
    [rect (top-left length width) (* length width)]
    [square (top-left length) (* length length)]))

(test (area c) 12.56)
(test (area s) 4)
(test (area r) 20)

; Number 6
(define (translate-shape s x)
  (type-case shape s
    [circle (center r)
            (circle (point (+ (point-x center) x) (point-y center)) r)]
    [rect (top-left length width)
          (rect (point (+ (point-x top-left) x) (point-y top-left)) length width)]
    [square (top-left length)
          (square (point (+ (point-x top-left) x) (point-y top-left)) length)]))

(test (translate-shape c 10)
      (circle (point 20 20) 2))

(test (translate-shape s 10)
      (square (point 35 0) 2))

(test (translate-shape r 10)
      (rect (point 40 15) 2 10))

; Number 7

(define (between? n b1 b2)
  (cond [(and (> n b1) (< n b2)) #t]
        [else #f]))

(define (in-shape? s p)
  (let ([px (point-x p)]
        [py (point-y p)])
  (type-case shape s
    ; (x - c.x)^2 + (y - c.x)^2 < r^2
    [circle (c r)
          (cond [(< (+ (* (- px (point-x c)) (- px (point-x c)))
                 (* (- py (point-y c)) (- py (point-y c))))
                 (* r r)) #t]
                [else #f])]
    [rect (tl length width)
          (cond [(and
                  (between? px (point-x tl) (+ (point-x tl) length))
                  (between? py (- (point-y tl) width) (point-y tl)))
                 #t]
                [else #f])]
    [square (tl length)
          (cond [(and
                  (between? px (point-x tl) (+ (point-x tl) length))
                  (between? py (- (point-y tl) length) (point-y tl)))
                 #t]
                [else #f])])))

(test (in-shape? c (point 11 20.5)) true)
(test (in-shape? c (point 21 22)) false)

; extra credit
; Number 1
(define (convert list-char ch)
  (cond [(empty? list-char) 0]
        [(eq? (first list-char) ch) (+ 1 (convert (rest list-char) ch))]
        [else (convert (rest list-char) ch)]))

; appends list-ch2 to end list-ch1
(define (append-helper list-ch1 list-ch2)
  (cond
    ; check if there is a space in the last character of list-ch1
    ; or if space in first character of list-ch2
    [(or
     (equal? (list-ref list-ch1 (- (length list-ch1) 1)) #\space)
     (equal? (list-ref list-ch2 0) #\space))
       (list->string (append list-ch1 list-ch2))]
    ; if not cons space to front of list-space2 and append resulting
    ; list to end of list-ch1, then convert back to string
    [else (list->string (append list-ch1 (cons #\space list-ch2)))]))

; appends str2 to end of str1 
(define (append-string str1 str2)
  (append-helper (string->list str1) (string->list str2)))
  
; extra credit 1 -> tree-map function 
(define (tree-map fcn ft)
  (cond
    [(unknown? ft) ft] ; check if unkown as base case
    [else (person (fcn (person-name ft)) ; apply fcn to (person-name ft)
                  (person-birthyear ft)
                  (person-eyecolor ft)
                  (tree-map fcn (person-mother ft)) ; recurse on mother tree
                  (tree-map fcn (person-father ft)))])) ; recurse on father tree

(define (add-last-names tree str-to-add)
  (tree-map (lambda (str) (
               append-string str str-to-add)) tree))

(test (add-last-names daves-tree "Smith")
      (person "Dave Smith" 1977 'brown
                   (person "Ken Smith" 1945 'brown
                                (unknown)
                                (unknown))
                   (person "Mary Ellen Smith" 1946 'brown
                                (unknown)
                                (unknown))))

(test (add-last-names daves-tree "Carl")
      (person "Dave Carl" 1977 'brown
                   (person "Ken Carl" 1945 'brown
                                (unknown)
                                (unknown))
                   (person "Mary Ellen Carl" 1946 'brown
                                (unknown)
                                (unknown))))


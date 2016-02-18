; Caroline Danzi
; CSE 465 Comparative Programming Languages
; Dr. Zmuda
; This program creates some functions in scheme and
; prints out the results.  

; zipcodes.scm contains all the US zipcodes.
; You should not modify this file. Your code
; should work for other instances of this file.
(load "zipcodes.scm")

; Helper function
(define (mydisplay value)
	(display value)
	(newline)
	#t
)

; Returns the roots of the quadratic formula, given
; ax^2+bx+c=0
(define (quadratic a b c)
	(cond 
		((= a 0) "error: cannot divide by zero")
		(else 
			(LET (
			(root_part_over_2a 
				(/ (sqrt (- (* b b) (* 4 a c))) (* 2 a)))
			(minus_b_over_2a (/ (- b 0) (* 2 a )))
			)
		(list (+ minus_b_over_2a root_part_over_2a) (- minus_b_over_2a root_part_over_2a))))
	)
	;(let (
	;	(bOver2a (/ (- 0 b) (* 2 a)))
	;	(sqrtOver2a (/ (sqrt (- (* b b) (* 4 a c))) (* 2 a)))
	;)
	
   ; '((/ (+ (- 0 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) 
	;(/ (- (- 0 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
	;(list (+ bOver2a sqrtOver2a) (- bOver2a sqrtOver2a))
	;)
	; (LET (
		; (root_part_over_2a 
			; (/ (sqrt (- (* b b) (* 4 a c))) (* 2 a)))
		; (minus_b_over_2a (/ (- b 0) (* 2 a )))
	; )
	; (list (+ minus_b_over_2a root_part_over_2a) (- minus_b_over_2a root_part_over_2a)))
)

(mydisplay (quadratic 1 0 0))
(mydisplay (quadratic 0 1 0)) 
(mydisplay (quadratic 3 4 2))

; Return a list with the items in reverse order
(define (reverse lst)
	(cond
		((NULL? lst) '())
		(else (append (reverse (cdr lst)) (list (car lst))))
	)
)

(mydisplay (reverse '(1 2 3 4)))

; Returns a list that is identical to lst, but with all
; instances of v1 replaced with v2.
; (replace '(a b c (c a b) b a) 'a 'b) -> (b b c (c b b) b b)
; lst -- list of items, possibly nested.
; v1 & v2 -- atoms
; 565 students only
(define (replace lst v1 v2)
	lst
)

(mydisplay (replace '(a b c c b a) 'a 'b))
(mydisplay (replace '(a b c (c a b) b a) 'a 'b))

; Returns a list of two numeric values. The first is the smallest
; in the list and the second is the largest in the list. 
; lst -- flat, contains numeric values, and length is >= 1.
(define (minAndMax lst)
	(list (minList lst) (maxList lst))
)

; Finds the smallest numeric value in a flat list
(define (minList lst)
	(cond
		((= (length lst) 1) (car lst))
		(else (min (car lst) (minList (cdr lst))))
	)
)

; Finds the largest numeric value in a flat list
(define (maxList lst) 
	(cond
		((= (length lst) 1) (car lst))
		(else (max (car lst) (maxList (cdr lst))))
	)
)

(mydisplay (minAndMax '(1 2 3 4)))
(mydisplay (minAndMax '(4 8 2 3)))

; Returns a list of three numbers (numNeg numZero numPos),
; where these numbers correspond to the number of negative
; numbers, number of zeros, and the number of positive numbers.
; For example (posneg '(-9 2 3 0 -2 -8 0)) should return
; (3 2 2). Approximately, 25% of this problem's points will be
; awarded for doing this with just one pass through the list.
; lst -- flat list containing numeric values, and length is >= 1.
(define (posneg lst)
	; Use tail recursive helper
	(posnegHelper lst 0 0 0)
)

; Uses tail recursion
(define (posnegHelper lst numNeg numZero numPos)
	(cond
		((NULL? lst) (list numNeg numZero numPos))
		; If the head of the list is negative
		((< (car lst) 0) (posnegHelper (cdr lst) (+ 1 numNeg) numZero numPos))
		; If the head of the list is zero
		((= (car lst) 0) (posnegHelper (cdr lst) numNeg (+ 1 numZero) numPos))
		; If the head of the list is positive
		(else (posnegHelper (cdr lst) numNeg numZero (+ 1 numPos)))
	)
)

(mydisplay (posneg '(1 2 3 4 2 0 -2 3 23 -3)))
(mydisplay (posneg '(-1 2 -3 4 2 0 -2 3 -23 -3 0 0)))
(mydisplay (posneg '()))

; The parameters are two flat lists with the same length.
; The inputs '(1 2 3) and '(a b c) should return a single list:
; ((1 a) (2 b) (3 c))
; lst1 & lst2 -- two flat lists with same length.
(define (zip lst1 lst2)
	(cond
		((NULL? lst1) '())
		(else (cons (list (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2))))
	)
)

(mydisplay (zip '("Smith" "Jackson" "Wilson") '(35 28 21)))

; Returns all the information for a particular zip code
; zipcode -- 5 digit integer
; zips -- the zipcode DB
(define (getZipcodeInfo zipcode zips)
	; caar zips gives the first element in the list at the head of the list,
	; which in this case is the numeric zipcode
	(if (= zipcode (caar zips))
		(car zips)
		(getZipcodeInfo zipcode (cdr zips))
	)
)

; Returns a list of all the states that contain the given place.
; The list of states should list the relevant states only once.
; placeName -- is the text corresponding to the name of the place
; zips -- the zipcode DB
(define (getStatesThatContainThisPlace placeName zips)
	(getStatesHelper placeName zips '())
	; not tail-recursive
	; (cond
		; ((NULL? zips) '())
		; ((EQUAL? placeName (cadar zips)) (cons (caddar zips) (getStatesThatContainThisPlace placeName (cdr zips)))) 
		; (else (getStatesThatContainThisPlace placeName (cdr zips)))
	; )
)

; Tail-recursive helper function
; Also, this lists the states in reverse alphabetical order...is that an issue?
(define (getStatesHelper placeName zips statesList)
	; cadar zips = the second element in the first nested list, which is the place name
	; caddar zips = the third element in the first nested list, which is the state name
	(cond
		((NULL? zips) statesList)
		((EQUAL? placeName (cadar zips)) (getStatesHelper placeName (cdr zips) (addNoDups (caddar zips) statesList)))
		(else (getStatesHelper placeName (cdr zips) statesList))
	)
)

; Add an element to the list if and only if the element does not
; already appear in the list
; TODO: Figure out why I currently need to pass in the original list
(define (addNoDups element lst)
	(if (MEMBER? element lst)
		lst
		(append lst (list element))
	)
)

(define (MEMBER? element lst)
	(cond
		((NULL? lst) #f)
		((EQUAL? element (car lst)) #t)
		(else (MEMBER? element (cdr lst)))
	)
)

; Returns the state that contains the most unique zip codes
; Return the first state if there is a tie
; zips -- zipcode DB
(define (getStateWithMostZipcodes zips)
	"AK"
)

; Count the number of zip codes for one state and add to a list (num, state)
; find the highest number and return the state

; Returns the distance between two zip codes.
; Use lat/lon. Do some research to compute this.
; zip2 -- zipcode DB
; zip1 & zip2 -- the two zip codes in question.
; 565 students only
(define (getDistanceBetweenZipCodes zips zip1 zip2)
	0
)
(mydisplay (getZipcodeInfo 45056 zipcodes))
(mydisplay (getStatesThatContainThisPlace "Oxford" zipcodes))
(mydisplay (getStateWithMostZipcodes zipcodes))
(mydisplay (getDistanceBetweenZipCodes zipcodes 45056 48122))


; Some sample predicates
(define (POS? x) (> x 0))
(define (NEG? x) (> x 0))
(define (LARGE? x) (>= (abs x) 10))
(define (SMALL? x) (NOT (LARGE? x)))

; Returns a list of items that satisfy a set of predicates.
; For example (filterList '(1 2 3 4 100) '(EVEN?)) should return the even numbers (2 4 100)
; (filterList '(1 2 3 4 100) '(EVEN? SMALL?)) should return (2 4)
; lst -- flat list of items
; filters -- list of predicates to apply to the individual elements
(define (filterList lst filters)
	(cond
		((NULL? lst) '())
		(((car filters) (car lst)) (cons (car lst) (filterList (cdr lst) filters)))
		(else (filterList (cdr lst) filters)) 
	)
	; runs through each filter in filters
	;(cond
	;	((NULL? filters) '())
	;	(else (filterList (filterHelper lst (car filters)) (cdr filters)))
	;)
)

; applies one filter and returns a filtered list
(define (filterHelper lst filter)
	(cond
		((NULL? lst) '())
		((filter (car lst)) (cons (car lst) (filterHelper (cdr lst) filter)))
		(else (filterHelper (cdr lst) filter)) 
	)
)

(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS?)))
;(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN?)))
;(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN? LARGE?)))
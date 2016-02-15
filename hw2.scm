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
(mydisplay (quadratic 1 1 0)) ; changed a from 0 to 1 - need to handle when a equals zero
(mydisplay (quadratic 3 4 2))

; Return a list with the items in reverse order
(define (reverse lst)
	lst
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
	'()
)

; Returns a list of three numbers (numNeg numZero numPos),
; where these numbers correspond to the number of negative
; numbers, number of zeros, and the number of positive numbers.
; For example (posneg '(-9 2 3 0 -2 -8 0)) should return
; (3 2 2). Approximately, 25% of this problem's points will be
; awarded for doing this with just one pass through the list.
; lst -- flat list containing numeric values, and length is >= 1.
(define (posneg lst)
	'()
)

(mydisplay (posneg '(1 2 3 4 2 0 -2 3 23 -3)))
(mydisplay (posneg '(-1 2 -3 4 2 0 -2 3 -23 -3 0 0)))
(mydisplay (posneg '()))

; The paramters are two flat lists with the same length.
; The inputs '(1 2 3) and '(a b c) should return a single list:
; ((1 a) (2 b) (3 c))
; lst1 & lst2 -- two flat lists with same length.
(define (zip lst1 lst2)
	'()
)

(mydisplay (zip '("Smith" "Jackson" "Wilson") '(35 28 21)))

; Returns all the information for a particular zip code
; zipcode -- 5 digit integer
; zips -- the zipcode DB
(define (getZipcodeInfo zipcode zips)
	'(99553 "Akutan" "AK" "Aleutians East" 54.143 -165.7854)
)

; Returns a list of all the states that contain the given place.
; The list of states should list the relevant states only once.
; placeName -- is the text corresponding to the name of the place
; zips -- the zipcode DB
(define (getStatesThatContainThisPlace placeName zips)
	'("AK")
)

; Returns the state that contains the most unique zip codes
; Return the first state if there is a tie
; zips -- zipcode DB
(define (getPlaceNameThatAppearsInMostStates zips)
	"Anytown"
)

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
(mydisplay (getPlaceNameThatAppearsInMostStates zipcodes))
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
	lst
)

(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) '(POS? EVEN? LARGE?)))
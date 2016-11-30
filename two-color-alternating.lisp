;GLOBAL PARAMETERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *guess* ())
(defparameter *previous-guess* ()); used when memory is needed. Not by all functions
(defparameter *possible-colors* ()) ;;list of possible colors in any order (cows, but may contain bulls)
(defparameter *previous-possible-colors* ()) ;;memory for *possible-colors*
(defparameter *possible-colors-combo* ()) ;;list of all possible color combination, used to guess
(defparameter *previous-possible-colors-combo* ()) ;;memory for *possible-colors-combo*
(defparameter *known-colors* ()) ;; colors known and in proper order (bulls)

;FUNCTIONS USED IN 'TWO-COLOR-ALTERNATING-SOLVER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;creates a list that contains all the consecutive 2-tuples,
;;e. g. (consecutive-two-tuples '(a b c d e)) -> ((a b) (b c) (c d) (d e)) 
(defun consecutive-two-tuples (colors)
	   (cond 
	     ((endp colors) ())
	     ((not (first (rest colors))) ())
	     (T (cons (list (first colors) (first (rest colors))) (consecutive-two-tuples (rest colors))))))

;;takes a color that is known to be in the solution and returns all possible guesses with that color, but not with itself.
;;Optionally, can be used with the position in the answer of the known color
;;e. g. (color-with-list-product 'a '(a b c)) -> ((a b) (a c))
(defun color-with-list-product (known &optional position)
  (setf *possible-colors* (remove known *possible-colors*))
  (cond
    ((eql position 1)
     (setf *possible-colors-combo* (loop for x in *possible-colors*
				collect (list known x))))
    ((eql position 2)
     (setf *possible-colors-combo* (loop for x in *possible-colors*
				collect (list x known))))
    (T
     (setf *possible-colors-combo* (loop for x in *possible-colors*
				append (list (list x known) (list known x)))))))

;;same as color-with-list-product but also removes the single color we know not to be part of the solution
;;e. g. (color-with-list-update 'a 'b '(a b c d e)) -> ((a c) (a d) (a e))
(defun color-with-list-product-helper (known not-part &optional position)
  (setf *possible-colors* (remove not-part *possible-colors*))
  (cond
    ((not position)
     (color-with-list-product known))
    (T
     (color-with-list-product known position))))


;;creates a list that contains every other 2-tuples
;;e. g. (every-other-two '(a b c d e) -> ((a b) (c d) (d e))
(defun every-other-two (colors)
	   (cond 
	     ((endp colors) ())
	     ((= (length (rest (rest colors))) 1) (cons (list (first colors) (second colors)) (every-other-two (rest colors))))
	     (T (cons (list (first colors) (first (rest colors))) (every-other-two (rest (rest colors)))))))

;;expands 2 possible choices alternatively to the number of pegs
;;e. g. (create-two-color-guess 6 '(a b)) -> (a b a b a b)
(defun create-two-color-guess (length colors)
	   (loop for i from 0 to (1- length)
	      append (list (nth (mod i 2) colors))))

(defun set-my-guess (length)
  (setf *guess* (create-two-color-guess length (first *possible-colors-combo*))))

(defun set-my-guess-and-decrement (length)
  (set-my-guess length)
  (when (consp (rest *possible-colors-combo*))
    (setf *possible-colors-combo* (rest *possible-colors-combo*))))

;;destructive replace
(defun my-replace (atom position list)
  (setf (nth position list) atom)
  list)

;TWO-COLOR-ALTERNATING-SOLVER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun two-color-alternating-solver (board colors SCSA last-response)
  (declare (ignore SCSA))
  (when (not last-response)
    ;;reset global var
    (setf *known-colors* '(nil nil))
    (setf *possible-colors* ())
    (setf *previous-possible-colors* ())
    (setf *possible-colors-combo* ())
    (setf *previous-possible-colors-combo* ())
    (setf *guess* ())
    (setf *previous-guess* '(nil nil));first is *guess* second is 0 or 1 depending if it was saved recently or not
    ;;initialize and first-guess case
    (setf *possible-colors* colors)
    (setf *possible-colors-combo* (consecutive-two-tuples colors))
    )

  (cond
    ;very first guess
    ((not last-response)
     (set-my-guess-and-decrement board)
     (setf (first *previous-guess*) nil)
     (setf (second *previous-guess*) 0))
     
    ;when *previous-guess* has been set, meaning we are trying combos from cows and we didn't hit the mark
    ;meaning (first *previous-guess*) was the cow -> it is actually (second *guess*) 
    ((and (eql (second *previous-guess*) 1) (eql (first last-response) 0))
     (setf *possible-colors* *previous-possible-colors*)
     (color-with-list-product-helper (first (first *previous-guess*)) (second (first *previous-guess*)) 2)
     (set-my-guess-and-decrement board)
     (setf (second *previous-guess*) 0))
    
    ;case when all cows and even board
    ((eql (second last-response) board)
     (setf *guess* (reverse *guess*)))
    
    ;case when all cows and odd board
    ((eql (second last-response) (1- board))
     (setf *guess* (cons (second *guess*) (reverse (rest *guess*)))))
    
    ;case when bulls as first color on odd board
    ((and (eql (first last-response) (ceiling (/ board 2.0))) (eql (mod board 2) 1))
     (color-with-list-product-helper (first *guess*) (second *guess*) 1)
     (set-my-guess-and-decrement board))
    
    ;case when bulls as second and odd board
    ((and (eql (first last-response) (floor (/ board 2.0))) (eql (mod board 2) 1))
     (color-with-list-product-helper (second *guess*) (first *guess*) 2)
     (set-my-guess-and-decrement board))
    
    ;case when we have half of the board as cows. Same for even or odd boards
    ((eql (second last-response) (floor (/ board 2.0)))
     (setf (first *previous-guess*) *guess*)
     (setf (second *previous-guess*) 1)
     (setf *previous-possible-colors* *possible-colors*)
     ;guess (second *guess*) is the cow -> it is first position
     (color-with-list-product-helper (second *guess*) (first *guess*) 1)
     (set-my-guess-and-decrement board))
    
    ;Half are bulls on even board
    ;To be continued

    (T
     (set-my-guess-and-decrement board)
     (print "T cond used")))
    
  
  ;guess output
  (print *guess*)
  *guess*)

;GLOBAL VARIABLES USED BY GENERAL-CASE-SOLVER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *possible-solutions* ())
(defparameter *possible-colors* ())

;FUNCTIONS USED BY GENERAL-CASE-SOLVER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This function has been adapted from code we found on github. Although we understand it and could reproduce
;it, it is not our work.
;It creates a cartesian product of one list with itself board-many times
(defun increment (colors-copy board)
  (if (= board 1) (loop for x in colors-copy collect (list x))
    (mapcan #'(lambda (x) (mapcar #'(lambda (y) (cons x y))
    (increment colors-copy (1- board)))) colors-copy)))

(defun compare (board last-response)
  (cond
    ;nothing form the guess is part of the answer
    ((and (eql (first last-response) 0) (eql (second last-response) 0))
     (loop for x in *guess*
	do (loop for y in *possible-solutions*
	      do (when (member x y)
		   (setf *possible-solutions* (remove y *possible-solutions*))))))
    ;We have all the colors but not in the right order (mix of bulls and cows)
    ;Nobel-prize-winning logic I know, but it's 3:18am and my LISP-fu isn't that great
    ((eql (+ (first last-response) (second last-response)) board)
     (loop for x in *guess*
	do (loop for y in *possible-solutions*
	      do (when (not (member x y))
		   (setf *possible-solutions* (remove y *possible-solutions*)))))
     (loop for y in *possible-solutions*
	do (loop for z in y
	      do (when (not (member z *guess*))
		   (setf *possible-solutions* (remove y *possible-solutions*))))))))
    
		   
     


       
;GENERAL-CASE-SOLVER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun general-case-solver (board colors SCSA last-response)
  (cond
    ;first guess
    ((not last-response)
     (setf *possible-colors* colors)
     (setf *possible-solutions* (increment colors board))
     (setf *guess* (append (loop for i from 1 to (ceiling (/ board 2.0))
			      append (list (first colors)))
			   (loop for i from 1 to (floor (/ board 2.0))
			      append (list (second colors))))))
    (T
     (compare board last-response)
     (setf *guess* (first *possible-solutions*))
     (if (consp (rest *possible-solutions*))
	 (setf *possible-solutions* (rest *possible-solutions*)))))
  *guess*)
    
     


;MAIN FUNCTION DMMG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DMMG (board colors SCSA last-response)
  (cond ((equal SCSA 'two-color-alternating)
	 (two-color-alternating-solver board colors SCSA last-response))
	(T
	 (general-case-solver board colors SCSA last-response))))
  
	    
	  
        
    

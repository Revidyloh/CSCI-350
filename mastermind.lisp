;;;*******************************************************************************************
;;;THIS FILE DEFINES THE GAME ENVIRONMENT AND PROVIDES A SAMPLE PLAYER THAT MAKES RANDOM MOVES
;;;*******************************************************************************************

;;Support functions
;;selects a random element from a list
(defun random-chooser (list)
  (nth (random (length list)) list))

;selects n distinct random elements from list)
(defun choose-n-random (n list)
  (if (> n (length list)) 
      (print 'error)
      (loop for i from 1 to n
	 for choices = (copy-list list) then (set-difference choices (list chosen))
	 for chosen = (random-chooser choices)
	 collect chosen)))

;returns the first number items of list
(defun firstn (number list)
  (loop for i from 1 to number
     for item in list
     collect item))

;converts color letters to numbers
;notice that T cannot be a letter here, so TT is used instead
(defun spot (color)
  (case color
    (A 0)
    (B 1)
    (C 2)
    (D 3)
    (E 4)
    (F 5)
    (G 6)
    (H 7)
    (I 8)
    (J 9)
    (K 10)
    (L 11)
    (M 12)
    (N 13)
    (O 14)
    (P 15)
    (Q 16)
    (R 17)
    (S 18)
    (TT 19)
    (U 20)
    (V 21)
    (W 22)
    (X 23)
    (Y 24)
    (Z 25)))

;;;*******************************************************************************************
;;Class definition and *Mastermind*
;;;*******************************************************************************************

;board is the number of pegs
;colors is a list of letters (for colors)
;colors is the number of colors in the game
;answer is the secret code
;SCSA is the name of the secret code generator
;guesses is how many guesses so far
;game-cutoff is the number of guesses permitted
;tournament-cutoff is the number of games in a tournament

;Mastermind;note default values for all slots 
(defclass game ()
  ((board :initarg :board :initform 0 :accessor board :documentation "number of pegs")
   (colors :initarg :colors :initform '(A B C D E F G H I J K L M N O P Q R S TT U V W X Y Z) :accessor colors :documentation "list of possible colors")
   (number-of-colors :initarg :number-of-colors :initform 0 :accessor number-of-colors :documentation "number of colors used")
   (answer :initarg :answer :initform nil :accessor answer :documentation "target the current round is trying to guess")
   (SCSA :initarg :SCSA :initform nil :accessor SCSA :documentation "name of generator for targets")
   (guesses :initarg :guesses :initform 0 :accessor guesses :documentation "history of guesses")
   (game-cutoff :initarg :game-cutoff :initform 100 :accessor game-cutoff :documentation "time after which a round is terminated")))

(defvar *Mastermind* (make-instance 'game))

;Creates a game called *Mastermind* on pegs pegs with colors colors
;secret code is created during tournament
(defun Mastermind (pegs hues SCSA)
  (declare (special *Mastermind*)) 
  (when (and (integerp pegs) (plusp pegs) (integerp hues) (plusp hues) (<= hues 26)) 
    (setf *Mastermind* (make-instance 'game :board pegs :number-of-colors hues :SCSA SCSA
				      :colors (firstn hues '(A B C D E F G H I J K L M N O P Q R S TT U V W X Y Z))))))

;;;*******************************************************************************************
;;PLAY
;;;*******************************************************************************************

;returns t if a guess is of the correct length and only contains valid colors, else nil
(defmethod valid-guess ((self game) guess)
  (and (listp guess) 
       (= (length guess) (board self))
       (loop with colors = (colors self) for item in guess
	    always (member item colors :test 'equal))))

;counts the number of each color in a guess into an array and returns the array
(defmethod color-counter ((self game) list)
  (loop with tally = (make-array (number-of-colors self) :initial-element 0)
     for peg in list
     for index = (spot peg)
     do (incf (aref tally index))
     finally (return tally)))

;scores a guess, returning a two-element list (#exact #other) where other means "right color, wrong location"
(defmethod process-guess ((self game) guess)
  (loop with answer = (answer self)
     with guess-color-count = (color-counter self guess)
     with true-color-count = (color-counter self answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     finally (return (list exact-counter (loop for i from 0 to (1- (number-of-colors self))
					    for guessed = (aref true-color-count i)
					    for true = (aref guess-color-count i)
					    when (<= true guessed)
					    sum true
					    else sum guessed)))))

;if the guess is the answer this returns the list (win #guesses), if the guess is invalid it returns nil
;otherwise it returns the guess followed by the score (#exact #other #guesses-so-far)
(defmethod respond-to-guess ((self game) guess i)
  (let* ((board (board self))
	 (valid (valid-guess self guess))
	 (score (when valid (process-guess self guess)))
	 (win (and score (= (first score) board))))
    (cond ((not valid)  nil)
	  (win (list 'win i))
	  (t (append score (list i))))))

;plays one round of game against team during which team gets up to game-cutoff guesses at the answer
;uncomment the format statements for a play-by-play version
;calls a function team that you will provide that makes a guess
;the name of your function should be the name of your team
;each round is limited to 10000 microseconds (unlimited time for a single guess)
;if you time out it counts as if your last guess scored (0 0)
(defmethod play-round ((self game) team)
  (declare (special *Mastermind*)) 
  (loop with game-cutoff = (game-cutoff self)
     with stop-time = (+ 10000000 (get-internal-run-time))
     with board = (board *Mastermind*)
     with colors = (colors *Mastermind*)
     with SCSA = (SCSA *Mastermind*)
     for i from 1 to game-cutoff
     for guess = (funcall team board colors SCSA nil) then (funcall team board colors SCSA response) 
     for response = (respond-to-guess self guess i)
     for win = (equal (first response) 'win)
     for time-is-up = (> (get-internal-run-time) stop-time)
     ;;do (print (list (get-internal-run-time) stop-time)) 
     when win ;;
     do (format t "~%Win. Round over.") ;;
     else when response ;;
     do (format t "~%score ~a" response) ;;
     else do (format t "~%Invalid entry. Round over.") ;;
     until (or win (null response) (= i game-cutoff) time-is-up)
     finally (return (cond (time-is-up '(0 0)) 
			   ((null response) nil)
			   (t response)))))

;Plays tournament-length rounds against one team
;a win is worth more if it takes fewer guesses (division by square root of number of guesses)
;ATTENTION: there are two ways to use this function.
;METHOD 1: If argument is a fixed list of pre-generated answers: Set the value of a variable to an SCSA-generated list and use that variable
;for example: (setf foo (SCSA-sampler 25 'two-color-alternating 7 5)) and then call (play-tournament *Mastermind* team-name foo 25)
;will rerun your team-name on the same data every time and help you debug
;METHOD 2: If argument is an SCSA: Generate a new list of boards on every run.
;for example, (play-tournament *Mastermind* team-name 'two-color-alternating 25))
;will run your team-name against a different set of freshly-generated codes every time, and help you do performance evaluation
;WARNING: if you use method 1, be sure you generate enough boards for your call to play-tournament (e.g., at least 25 here)
(defmethod play-tournament ((self game) team argument number-of-games)
  (declare (special *Mastermind*)) (print team)
  (loop with wins = 0
     with failures = 0
     with rounds = 0
     with codes = (if (listp argument) argument (SCSA-sampler number-of-games argument (board self) (number-of-colors self)))
     for i from 0 to (1- number-of-games)
     for round = (and (setf (answer *Mastermind*) (nth i codes)) (play-round self team)) ;this is where the; code is set
    ;do (print (nth i codes))
    ; when (= (* 10 (floor (/ i 10))) i) do (print i)
     when (equal (first round) 'win)
     do (incf wins (float (/ 1 (expt (second round) .5))))
     else when (null round)
     do (incf failures) 
     else do (incf rounds)
     finally (print (list 'score (scoring-function (list wins rounds failures))))
       (return (list wins rounds failures))))

;you get 5 points for a full win and -2 for every round that ended in an invalid guess
(defun scoring-function (list)
  (+ (* 5 (first list)) (* -2 (third list))))
       
;;;*******************************************************************************************
;;Sample SCSAs
;;;*******************************************************************************************

;to see what codes from a specific SCSA look like, use this function
;for example (scsa-sampler 100 'first-and-last 6 8) will give 100 samples for 6 pegs with 8 colors using the SCSA first-and-last
;collects and returns number of secret codes generated by SCSA for pegs pegs and colors colors
(defun SCSA-sampler (number SCSA pegs colors)
  (loop for i from 1 to number
       collect (funcall SCSA pegs (firstn colors '(A B C D E F G H I J K L M N O P Q R S TT U V W X Y Z)))))

;makes a list of length length containing colors selected at random from colors
(defun insert-colors (length colors)
  (loop for i from 1 to length
     collect (random-chooser colors)))

;makes a 2-color list 
(defun two-color (length colors)
  (loop with choices = (choose-n-random 2 colors)
     for i from 1 to length 
     collect (random-chooser choices)))

;makes a list of only As and Bs
(defun ab-color (length colors)
  (declare (ignore colors))
  (loop with choices = '(A B)
     for i from 1 to length
     collect (random-chooser choices)))

;makes a list that alternates 2 colors
(defun two-color-alternating (length colors)
  (loop with choices = (choose-n-random 2 colors) 
     with first-color = (first choices)
     with second-color = (second choices)
     for i from 1 to length 
     when (oddp i) 
     collect first-color
     else collect second-color))  

;makes a list in which a color appears at most once
(defun only-once (length colors)
  (if (< (length colors) length) 
      (break)
      (loop for i from 1 to length
	 for color-list = (copy-list colors) then (set-difference color-list (list choice))
	 for choice = (random-chooser color-list)
	 collect choice)))

;makes a list in which the first and last colors are the same
(defun first-and-last (length colors)
  (loop with first = (random-chooser colors)
       for i from 2 to (1- length)
       collect (random-chooser colors) into ans
       finally (return (append (cons first ans) (list first)))))

;makes a list that usually has fewer (2 or 3) colors
(defun usually-fewer (length colors)
  (let* ((probability (random 100))
	 (coin-flip (when (< probability 90) (random-chooser '(2 3))))
	 (choices (cond (coin-flip (if (= 3 coin-flip) (choose-n-random 3 colors) (choose-n-random 2 colors)))
			(t colors))))
    (loop for i from 1 to length
       collect (random-chooser choices))))

;makes a list with preferences for fewer colors
(defun prefer-fewer (length colors)
  (let* ((probability (random 100))
	 (color-count (length colors))
	 (coin-flip (cond ((<= probability 49) 1)
		      ((<= probability 74) 2)
		      ((<= probability 87) (if (>= color-count 3) 3 color-count))
		      ((<= probability 95) (if (>= color-count 4) 4 color-count))
		      ((<= probability 98) (if (>= color-count 5) 5 color-count))
		      (t (if (>= color-count 6) (random-chooser (loop for i from 6 to color-count 
								   collect i)) color-count))))
	 (choices (choose-n-random coin-flip colors))) 
    (loop for i from 1 to length
       collect (random-chooser choices)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mystery-1 (length colors)
  (loop with choices = (choose-n-random 3 colors) 
     
     with first-color = (first choices)
     with second-color = (second choices)
     with third-color =(third choices)
     
     for i from 1 to length 
     when (or (equal i 1) (equal (mod (+ i 2) 3)0)) 
     collect first-color
     when (or (equal (mod (- i 2)3) 0)(equal i 2))
     collect second-color
     when (equal (mod i 3) 0)
     collect third-color))

;;;*******************************************************************************************
;;Sample teams
;;;*******************************************************************************************
;this is a really dumb team... it makes random guesses
(defun RandomFolks (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
  (insert-colors board colors))

;this isn't much better.... it guesses all the same color, and chooses that color at random
(defun Boring (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
    (make-list board :initial-element (random-chooser colors)))


(defun increment (colors-copy board)
  (if (= board 1) ;;checks if we have filled the board by reducing its value to "1"
      (loop for x in colors-copy collect (list x)) ;; when we have filled the board we collect and return all the possible guesses
    (mapcan #'(lambda (x) ;; x is created with a placeholder function to hold each guesses
    (mapcar #'(lambda (y) (cons x y)) ;;mapcar puts each of our colors "y" into elements/guesses of the accumulated list(ex. a is added to (a) (b) (c) to make (a a) (b b) (c c)...)
    (increment colors-copy (1- board)))) ;; recursively expands the list of guesses so that each color can be added to each list yet again
     colors-copy)));;mapcan puts each guess generated into our color-copy list

;;;Team DMMG(Dan, Mike, Matt, Gwenael) 
;;;Artificial Intelligence Project baseline Mastermind Player


(defparameter *counter* 0) ;;establish a global counter to iterate through list of combinations generated by increment
(defun baseline1 (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let ((colors-copy colors) (guess ())) ;;create a copy-colors (for increment) and a guess
    (if (equal last-response nil) (progn (setf *counter* 1) (setf guess (make-list board :initial-element 'A))) ;;should only evaluate to true for first guess
      (progn (setf guess (nth *counter* (increment colors-copy board))) (incf *counter*))) ;;extract the nth element of the increment function, counter++
    guess)) ;;return the guess

;;;;;;;;;;;;;;;;;;;;;; BASELINE2

(defparameter *counter2* 0) ;;for monochromatic iteration
(defparameter *counter3* 0) ;;for increment
(defparameter *in-answer* ()) ;;store all colors in answers
(defparameter *store-mono* ()) ;;store monochromatics to be used with last-response
(defun baseline2 (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let ((guess ()) (previous-guess ()) (check 0))
    (when (equal last-response nil) (progn (setf *counter2* 0) (setf *counter3* 0) (setf *in-answer* ()) (setf *store-mono* ()))) ;;reset everything after each round
    (if (= *counter2* (length colors)) (setf check 1) ;;if we went through all monochromatics, skip the below and proceed onward
      (progn (when (equal last-response nil) (setf *store-mono* (loop for x in colors collect (make-list board :initial-element x)))) ;;generate all monochromatics, should only happen once
      (cond ((equal last-response nil) (progn (setf guess (nth *counter2* *store-mono*)) (setf *counter2* 1))) ;;should only evaluate to true on first guess (all A's)
        (t (progn (setf guess (nth *counter2* *store-mono*)) (setf previous-guess (nth (- *counter2* 1) *store-mono*)) (incf *counter2*))))
      (when (not (equal last-response nil)) (unless (= (first last-response) 0) (push (first previous-guess) *in-answer*))))
    )
    
    (when (and (equal (first (last colors)) (first (first (last *store-mono*)))) (= check 1)) (progn 
      (unless (= (first last-response) 0) (push (first (first (last *store-mono*))) *in-answer*)) (setf *store-mono* '()))) ;;evaluate the last monochromatic
      ;;should only run once per round, delete *store-mono*

    (if (= check 0) guess ;;should only evaluate to true for monochromatics
      (progn (setf guess (nth *counter3* (increment *in-answer* board))) (incf *counter3*))) guess))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BASELINE3

(defparameter *counter2* 0) ;;for monochromatic iteration
(defparameter *counter3* 0) ;;for increment
(defparameter *in-answer* ()) ;;store all colors in answers
(defparameter *store-mono* ()) ;;store monochromatics to be used with last-response
(defparameter *color-freq* ()) ;;store the frequency of each color
(defparameter *combos* ()) ;;store all combinations of colors
(defparameter *bull-count* 0) ;;store the bull count 
(defun baseline3 (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let ((guess ()) (previous-guess ()) (check 0))
    (when (equal last-response nil) (progn (setf *counter2* 0) (setf *counter3* 0) (setf *bull-count* 0) (setf *combos* ()) (setf *color-freq* ()) (setf *in-answer* ()))) ;;reset everything after each round
    (cond ((or (= *counter2* (length colors)) (= *bull-count* board)) (setf check 1)) ;;if we went through all monochromatics, skip the below and proceed onward
      ((equal last-response nil) (progn (setf *store-mono* (loop for x in colors collect (make-list board :initial-element x))) ;;create all monochromatics, should only happen once per round
          (setf guess (nth *counter2* *store-mono*)) (incf *counter2*))) ;;set the guess to A/AA/AAA/etc. and counter2 to 1
      (t (progn (setf guess (nth *counter2* *store-mono*)) (setf previous-guess (nth (- *counter2* 1) *store-mono*)) (incf *counter2*) ;;set the current guess and previous guess
        (unless (= (first last-response) 0) (progn (push (first previous-guess) *in-answer*) ;;if the bull count = 0 from last response, ignore this code
          (loop for x from 1 to (first last-response) do (progn (push (first previous-guess) *color-freq*) (incf *bull-count*)))))))) ;;if != 0, put color into in-answer, and obtain its frequency within the answer
          ;;example: in-answer -> (A B C); color-freq -> (A A B C C) = two As, 1 B, 2 Cs

    (if (= *bull-count* board) (setf check 1)) ;;remove extra non-answer monochromatic
    
    (cond ((= check 0) guess) ;;if it's a monochromatic, make the guess (only evaluates to true when guess = monochromatic)
      ((and (> (length *store-mono*) 1) (= check 1)) (progn ;;deal with the last monochromatic
        (unless (= *bull-count* board) (progn (push (first (first (last *store-mono*))) *in-answer*) ;;if the last monochromatic isn't 0, add it to in-answer and collect its frequency
          (loop for x from 1 to (first last-response) do (push (first (first (last *store-mono*))) *color-freq*))))
          ;;----we're done with evaluating the monochromatics----
            (setf *store-mono* ()) (setf *combos* (increment *in-answer* board)) ;;clear the store-mono list (so we don't deal with it again until next round) and create the list of all combinations based on in-answer
              (loop for x in *combos* do (loop for y in *in-answer* do (when (not (= (count y *color-freq*) (count y x))) ;;compare the frequency of a color to the frequency in a possible combo
                (delete x *combos*)))) (setf *color-freq* '()) (incf *counter3*) (setf guess (nth *counter3* *combos*))) guess) ;;if no match, delete it from combos (repeat until we exhaustive through combos), make a guess
                ;;example: answer = AABC; ABBC would be deleted because only A-freq < 2, B-freq > 1, ABAC is not deleted because it has 2 As, 1 B and 1 C
      (t (progn (incf *counter3*) (setf guess (nth *counter3* *combos*))) guess)))) ;;make a guess based on the altered combos


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *counter* 0)
(defparameter *guess* ())
(defparameter *bull-check* 1)
(defparameter *in-answer* ())
(defparameter *not-in-answer* ())
(defparameter *check* 0)
(defparameter *counter2* 0)
(defun DMMG (board colors SCSA last-response)
  (when (equal SCSA 'ab-color) (ab-color-guesser board colors SCSA last-response))
  (when (equal SCSA 'only-once) (only-once-guesser board colors SCSA last-response))
  (when (equal SCSA 'first-and-last) (first-and-last-guesser board colors SCSA last-response))
  (when (equal SCSA 'two-color) (two-color-guesser board colors SCSA last-response))
  *guess*) 

(defun ab-color-guesser (board colors SCSA last-response)
  (declare (ignore SCSA colors)) ;;ignore SCSA and colors (we don't care about all of the colors, it's only A and B)
  (when (equal last-response nil) (progn (setf *counter* 0) (setf *guess* ()) (setf *bull-check* 1))) ;;reset everything each round
  (cond ((equal last-response nil) (progn (setf *guess* (make-list board :initial-element 'C)) (setf (nth *counter* *guess*) 'A))) ;;set all elements to C and then the first element to A
    ((= *bull-check* (first last-response)) (progn (incf *bull-check*) (incf *counter*) (setf (nth *counter* *guess*) 'A))) ;;if the previous guess's bull count = bull-check, ++bull-check and set
      ;;the next element to A
    (t (progn (setf (nth *counter* *guess*) 'B) (incf *bull-check*) (incf *counter*)))) ;;else, the current element is B 
  (if (< *counter* board) (setf (nth *counter* *guess*) 'A)) *guess*) ;;set the next element to A and make the guess
    ;;example: answer is ABABA, first guess: ACCCC -> (1,0) ++bull-check and ++counter, AACCC -> (1,0) bull-count != bull-check, this must mean the second element is a B, ++bull-check
    ;;++counter, ABACC -> (3,0) ++bull-check and ++counter, ABAAC (3,0) bull-count != bull-check, fourth element must be a B, ABABA -> (5,0) winner! 

(defun only-once-guesser (board colors SCSA last-response)
  (declare (ignore SCSA)) ;;ignore SCSA 
  (when (equal last-response nil) (progn (setf *counter* 0) (setf *counter2* 0) (setf *check* 0) (setf *not-in-answer* ()) (setf *in-answer* ()) (setf *guess* ()) (setf *bull-check* 1))) ;;reset everything each round
  (when (and (= board (length colors)) (= *check* 0)) (progn (setf *check* 3) (setf *counter* (+ (length colors) 1)) (loop for i in colors do (push i *in-answer*)) (setf *guess* (make-list board :initial-element(first (last *in-answer*)))))) 
  ;;when board = colors, push all colors into in-answer and skip to ONLY-ONCE (counter is set to length of colors + 1 to bypass a few conditionals), push all colors into in answer and make the last of in-answer as the pivot element
  ;;MONOCHROMATIC GENERATION
  (when (and (equal last-response nil) (= *check* 0)) (setf *guess* (make-list board :initial-element 'A))) ;;last response is nil, make the guess all As
  (when (and (not (equal last-response nil)) (= *check* 0)) (setf *guess* (make-list board :initial-element (nth *counter* colors)))) ;;else it's not nil, we make the guess all Bs, Cs, etc.
  ;;END MONOCHROMATIC GENERATION
  (cond ((and (equal last-response nil) (= *check* 0)) (incf *counter*)) ;;with the very first guess (As), just ++ the counter
      ((= *check* 3) (setf *check* 3)) ;;we are in the special case of board = colors
        ((and (= (first last-response) 0) (= *check* 0)) (progn (push (nth (- *counter* 1) colors) *not-in-answer*) (incf *counter*))) ;;if the previous guess yielded no bull count, add it to not-in-answer and ++ counter
          ((and (not (= (first last-response) 0)) (= *check* 0)) (progn (push (nth (- *counter* 1) colors) *in-answer*) (incf *counter*))) ;;else, the previous guess yielded a bull count, add it to in-answer and ++ counter
            (t (setf *check* 2))) ;;this is crucial so check doesn't change its value in order for the only-once part to run, IF CHECK = 2, WE ARE DOING ONLY-ONCE
  (when (and (> *counter* (length colors)) (not (= *check* 3)) (not (= *check* 2))) (setf *check* 1)) ;;we are done with monochromatics, begin the first step of only-once
  (when (and (<= *counter* (length colors)) (not (= *check* 3)) (not (= *check* 2))) (return-from only-once-guesser *guess*)) ;;we're still dealing with monochromatics, return the mono-guess and continue on
  (when (and (= *check* 3) (equal last-response nil)) (progn (setf *counter* 0) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*)) (return-from only-once-guesser *guess*))) ;;return the first guess for when board = colors
  ;;DONE WITH MONOCHROMATICS <--- this phase only begins once check is 1 or 3, when check is 2 or 3 we are in the process of only-once
  ;;;;BEGIN ONLY-ONCE
  (cond ((and (= *check* 3) (equal (nth *counter* *guess*) (first (last *in-answer*))) (= *bull-check* (first last-response))) ;;we are in the case where board = length of colors, the color = the pivot element
      (progn (delete (nth *counter2* *in-answer*) *in-answer*) (setf *counter2* 0) (incf *counter*) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*))))
    ((and (= *check* 3) (= (+ *bull-check* 1) (first last-response))) (progn (delete (nth *counter2* *in-answer*) *in-answer*) (setf *counter2* 0) (incf *counter*) (incf *bull-check*) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*))))
    ;;the case where board = length of colors, we always check bull-check + 1 because the pivot element IS in the answer so the default guess will always return at least 1 bull
    ((and (= *check* 3) (not (= (+ *bull-check* 1) (first last-response)))) (progn (incf *counter2*) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*))))
    ;;the case where board = length of colors, if the previous check doesn't yield bull count + 1, increment the selected element by choosing the next element in in-answer
      ((= *check* 1) (progn (setf *check* 2) (setf *guess* (make-list board :initial-element (first *not-in-answer*))) (setf *counter* 0) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*))) (return-from only-once-guesser *guess*))
      ;;we are in the case where the board != length of colors, once we're done with the monochromatics, establish the first guess by filling the guess with an element NOT in the answer and begin iteration
        ((= *bull-check* (first last-response)) (progn (delete (nth *counter2* *in-answer*) *in-answer*) (setf *counter2* 0) (incf *counter*) (incf *bull-check*) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*))))
        ;;once we find a position, we delete that position's colors from in-answer so we don't have to use it in iteration for other positions anymore
          (t (progn (incf *counter2*) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*))))) *guess*)
  ;;EXAMPLE: 5 pegs, 7 colors: answer is BAECD, in-answer: A B C D E, not-in-answer: F G, first guess is AFFFF -> returns (0,0), next guess is BFFFF -> returns (1,0) and delete B from in-answer, etc. until we get the answer
  ;;ANOTHER EXAMPLE: 5 pegs, 5 colors: answer is ABEDC, in-answer: A B C D E, not-in-answer is NOT used, first guess is AEEEE, we want the guess to return (2,0) (which it does), delete A from in-answer, next guess
  ;;is ABEEE, we want the guess to return (3,0) (which it does), delete B from in-answer, etc. bull-count is checked as being bull-count + 1 because the pivot element IS in the answer

  (defun first-and-last-guesser (board colors SCSA last-response)
    (declare (ignore SCSA))
    (let ((previous-guess ()) (temp-count 0))
    (when (equal last-response nil) (progn (setf *bull-check* 3) (setf *counter2* 0) (setf *guess* ()) (setf *counter3* 1) (setf *check* 0) (setf *bull-count* 0) (setf *color-freq* ()) (setf *not-in-answer* ()) (setf *in-answer* ()))) ;;reset everything after each round
    (cond ((= *check* 2) (setf *check* 2)) ((= *check* 3) (setf *check* 3))
      ((or (= *counter2* (length colors)) (= *bull-count* board)) (setf *check* 1)) ;;if we went through all monochromatics, skip the below and proceed onward
        ((equal last-response nil) (progn (setf *store-mono* (loop for x in colors collect (make-list board :initial-element x))) ;;create all monochromatics, should only happen once per round
          (setf *guess* (nth *counter2* *store-mono*)) (incf *counter2*))) ;;set the guess to A/AA/AAA/etc. and counter2 to 1
        (t (progn (setf *guess* (nth *counter2* *store-mono*)) (setf previous-guess (nth (- *counter2* 1) *store-mono*)) (incf *counter2*) ;;set the current guess and previous guess
          (if (not (= (first last-response) 0)) (progn (push (first previous-guess) *in-answer*) ;;if the bull count = 0 from last response, ignore this code
            (loop for x from 1 to (first last-response) do (progn (push (first previous-guess) *color-freq*) (incf *bull-count*)))) (push (first previous-guess) *not-in-answer*))))) ;;if != 0, put color into in-answer, and obtain its frequency within the answer
          ;;example: in-answer -> (A B C); color-freq -> (A A B C C) = two As, 1 B, 2 Cs

    (if (and (= *bull-count* board) (not (= *check* 3)) (not (= *check* 2))) (setf *check* 1)) ;;remove extra non-answer monochromatic
    
    (cond ((= *check* 0) *guess*) ;;if it's a monochromatic, make the guess (only evaluates to true when guess = monochromatic)
      ((= *check* 1) (progn (unless (= *bull-count* board) (progn (push (first (first (last *store-mono*))) *in-answer*) 
          (loop for x from 1 to (first last-response) do (push (first (first (last *store-mono*))) *color-freq*))))
          ;;----we're done with evaluating the monochromatics----
             (setf *check* 2) (setf *counter2* 0) (setf *guess* (make-list board :initial-element (first *not-in-answer*))) (setf (nth 0 *guess*) (nth *counter2* *in-answer*)) ;;begin the first and last check
              (setf (nth (- board 1) *guess*) (nth *counter2* *in-answer*)) (incf *counter2*)) *guess*) 
      ((and (= *check* 2) (not (= (first last-response) 2))) (progn (setf (nth 0 *guess*) (nth *counter2* *in-answer*)) (setf (nth (- board 1) *guess*) (nth *counter2* *in-answer*)) (incf *counter2*)) *guess*)
      ;;if we don't generate a 2 (meaning not the first and last), increment counter2 (for in-answer) and try another color
      ((and (= *check* 2) (= (first last-response) 2)) (progn (setf temp-count (count (nth (- *counter2* 1) *in-answer*) *color-freq*)) ;;we matched the first and last, proceed with the middle part now
        (if (<= temp-count 2) (progn (delete (nth (- *counter2* 1) *in-answer*) *color-freq*) (delete (nth (- *counter2* 1) *in-answer*) *in-answer*)) (delete (nth (- *counter2* 1) *in-answer*) *color-freq* :count temp-count))
        (setf *check* 3) (setf *counter2* 0) (setf (nth 1 *guess*) (nth *counter2* *in-answer*)) (incf *counter2*)) *guess*) ;;if the first and last color appeared at most 2 times, delete it from in-answer since it does not appear again
        ;;else, subtract "temp-count" amount of the first and last color because it appears at least one more time in the middle portion
      ((and (= *check* 3) (not (= (first last-response) *bull-check*))) (progn (setf (nth *counter3* *guess*) (nth *counter2* *in-answer*)) (incf *counter2*)) *guess*) ;;work with the middle portion now
      ((and (= *check* 3) (= (first last-response) *bull-check*)) (progn (loop for i in *guess* do (if (and (member i *in-answer*) (= (count i *guess*) (count i *color-freq*))) (delete i *in-answer*)))
        (setf *counter2* 0) (incf *counter3*) (setf (nth *counter3* *guess*) (nth *counter2* *in-answer*)) (incf *bull-check*) (incf *counter2*)) *guess*)
        ;;we always check each color in guess to make sure it has reached its limit in color frequency 
      (t nil)))) ;;<--- this should never evaluate
  ;;EXAMPLE: 5 pegs 7 colors: answer is BADEB, in-answer: A B D E, not-in-answer: C F G, try ACCCA (0 bulls), try BCCCB (2 bulls, we found our first and last), DELETE B from in-answer since it appears only 2 times in
  ;;the answer and proceed to the center portion, try BACCB (3 bulls) and now DELETE A because it appears only once in the answer (why bother guessing A again?), etc. etc. until you get the answer


  (defun two-color-guesser (board colors SCSA last-response)
    (declare (ignore SCSA))
    (let ((previous-guess ()))
    (when (equal last-response nil) (progn (setf *bull-check* 1) (setf *counter2* 0) (setf *guess* ()) (setf *check* 0) (setf *bull-count* 0) (setf *color-freq* ()) (setf *not-in-answer* ()) (setf *in-answer* ()))) ;;reset everything after each round
    (cond ((= *check* 2) (setf *check* 2))
      ((or (= *counter2* (length colors)) (= *bull-count* board)) (setf *check* 1)) ;;if we went through all monochromatics, skip the below and proceed onward
        ((equal last-response nil) (progn (setf *store-mono* (loop for x in colors collect (make-list board :initial-element x))) ;;create all monochromatics, should only happen once per round
          (setf *guess* (nth *counter2* *store-mono*)) (incf *counter2*))) ;;set the guess to A/AA/AAA/etc. and counter2 to 1
        (t (progn (setf *guess* (nth *counter2* *store-mono*)) (setf previous-guess (nth (- *counter2* 1) *store-mono*)) (incf *counter2*) ;;set the current guess and previous guess
          (if (not (= (first last-response) 0)) (progn (push (first previous-guess) *in-answer*) ;;if the bull count = 0 from last response, ignore this code
            (loop for x from 1 to (first last-response) do (progn (push (first previous-guess) *color-freq*) (incf *bull-count*)))) (push (first previous-guess) *not-in-answer*))))) ;;if != 0, put color into in-answer, and obtain its frequency within the answer

    (if (and (= *bull-count* board) (not (= *check* 2))) (setf *check* 1)) ;;remove extra non-answer monochromatic

    (cond ((= *check* 0) *guess*)
      ((= *check* 1) (progn 
        (unless (= *bull-count* board) (progn (push (first (first (last *store-mono*))) *in-answer*) ;;if the last monochromatic isn't 0, add it to in-answer and collect its frequency
          (loop for x from 1 to (first last-response) do (push (first (first (last *store-mono*))) *color-freq*))))
        (setf *check* 2) (setf *counter2* 0) (setf *guess* (make-list board :initial-element (first *not-in-answer*)))))
      ((and (= *check* 2) (= *bull-check* (first last-response))) (progn (incf *bull-check*) (incf *counter2*) (setf (nth *counter2* *guess*) (first *in-answer*))))
      ((and (= *check* 2) (not (= *bull-check* (first last-response)))) (setf (nth *counter2* *guess*) (second *in-answer*)) (incf *bull-check*) (incf *counter2*))
      (t nil))
    (if (and (not (= *check* 0)) (not (= *check* 1)) (< *counter2* board)) (setf (nth *counter2* *guess*) (first *in-answer*)) *guess*)))
  ;;this is very similar to ab-color except that it checks for monochromatics because we do not know the initial colors in the answer
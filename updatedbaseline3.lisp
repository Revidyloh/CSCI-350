;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BASELINE3

(defparameter *counter2* 0) ;;for monochromatic iteration
(defparameter *counter3* 0) ;;for increment
(defparameter *in-answer* ()) ;;store all colors in answers
(defparameter *store-mono* ()) ;;store monochromatics to be used with last-response
(defparameter *color-freq* ()) ;;store the frequency of each color
(defparameter *combos* ())
(defun baseline3 (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let ((guess ()) (previous-guess ()) (check 0))
    (when (equal last-response nil) (progn (setf *counter2* 0) (setf *counter3* 0) (setf *combos* ()) (setf *color-freq* ()) (setf *in-answer* ()))) ;;reset everything after each round
    (cond ((= *counter2* (length colors)) (setf check 1)) ;;if we went through all monochromatics, skip the below and proceed onward
      ((equal last-response nil) (progn (setf *store-mono* (loop for x in colors collect (make-list board :initial-element x))) ;;create all monochromatics, should only happen once per round
          (setf guess (nth *counter2* *store-mono*)) (setf *counter2* 1))) ;;set the guess to A/AA/AAA/etc. and counter2 to 1
      (t (progn (setf guess (nth *counter2* *store-mono*)) (setf previous-guess (nth (- *counter2* 1) *store-mono*)) (incf *counter2*) ;;set the current guess and previous guess
        (unless (= (first last-response) 0) (progn (push (first previous-guess) *in-answer*) ;;if the bull count = 0 from last response, ignore this code
          (loop for x from 1 to (first last-response) do (push (first previous-guess) *color-freq*))))))) ;;if != 0, put color into in-answer, and obtain its frequency within the answer
          ;;example: in-answer -> (A B C); color-freq -> (A A B C C) = two As, 1 B, 2 Cs
    
    (cond ((= check 0) guess) ;;if it's a monochromatic, make the guess (only evaluates to true when guess = monochromatic)
      ((and (> (length *store-mono*) 1) (= check 1)) (progn ;;deal with the last monochromatic
        (unless (= (first last-response) 0) (progn (push (first (first (last *store-mono*))) *in-answer*) ;;if the last monochromatic isn't 0, add it to in-answer and collect its frequency
          (loop for x from 1 to (first last-response) do (push (first (first (last *store-mono*))) *color-freq*))))
          ;;----we're done with evaluating the monochromatics----
            (setf *store-mono* '()) (setf *combos* (increment *in-answer* board)) ;;clear the store-mono list (so we don't deal with it again until next round) and create the list of all combinations based on in-answer
              (loop for x in *combos* do (loop for y in *in-answer* do (when (not (= (count y *color-freq*) (count y x))) ;;compare the frequency of a color to the frequency in a possible combo
                (delete x *combos*)))) (setf *color-freq* '()) (setf guess (nth *counter3* *combos*)) (incf *counter3*)) guess) ;;if no match, delete it from combos (repeat until we exhaustive through combos), make a guess
                ;;example: answer = AABC; ABBC would be deleted because only A-freq < 2, B-freq > 1, ABAC is not deleted because it has 2 As, 1 B and 1 C
      (t (progn (setf guess (nth *counter3* *combos*)) (incf *counter3*)) guess)))) ;;make a guess based on the altered combos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BASELINE3

(defparameter *counter2* 0) ;;for monochromatic iteration
(defparameter *counter3* 0) ;;for increment
(defparameter *in-answer* ()) ;;store all colors in answers
(defparameter *store-mono* ()) ;;store monochromatics to be used with last-response
(defparameter *color-freq* ()) ;;store the frequency of each color
(defparameter *store-guess* ()) ;;store all guesses
(defun baseline3 (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let ((guess ()) (previous-guess ()) (check 0))
    (when (equal last-response nil) (progn (setf *counter2* 0) (setf *counter3* 0) (setf *combos* ()) (setf *color-freq* ()) (setf *store-guess* ()) (setf *in-answer* ()))) ;;reset everything after each round
    (if (= *counter2* (length colors)) (setf check 1) ;;if we went through all monochromatics, skip the below and proceed onward
      (progn (when (equal last-response nil) (setf *store-mono* (loop for x in colors collect (make-list board :initial-element x)))) ;;generate all monochromatics, should only happen once
      (cond ((equal last-response nil) (progn (setf guess (nth *counter2* *store-mono*)) (setf *counter2* 1))) ;;should only evaluate to true on first guess (all A's)
        (t (progn (setf guess (nth *counter2* *store-mono*)) (setf previous-guess (nth (- *counter2* 1) *store-mono*)) (incf *counter2*))))
      (when (not (equal last-response nil)) 
        (unless (= (first last-response) 0) (progn (push (first previous-guess) *in-answer*) (loop for x from 1 to (first last-response) do (push (first previous-guess) *color-freq*)))))))
    
    (when (and (> (length *store-mono*) 1) (= check 1)) (progn 
      (unless (= (first last-response) 0) (progn (push (first (first (last *store-mono*))) *in-answer*) (loop for x from 1 to (first last-response) do (push (first (first (last *store-mono*))) *color-freq*)))) 
        (setf *store-mono* '()) (setf check 2))) ;;evaluate the last monochromatic
      ;;should only run once per round, delete *store-mono*

    (when (= check 2) (progn (setf *combos* (increment *in-answer* board)) 
      (loop for x in *combos* do (loop for y in *in-answer* do (when (not (= (count y *color-freq*) (count y x))) (delete x *combos*)))) (setf *color-freq* '())))
      ;;delete all instances where the combo doesn't match the answer (i.e. if the answer is ABBA, we delete all *combos* where B and A counts are less than or greater than 2)
      ;;this should only run once per round, delete *color-freq*

    (if (= check 0) guess ;;should only evaluate to true for monochromatics
      (progn (setf guess (nth *counter3* *combos*)) (incf *counter3*))) guess))
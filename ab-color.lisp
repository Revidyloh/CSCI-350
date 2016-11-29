(defparameter *counter* 0)
(defparameter *guess* ())
(defparameter *bull-check* 1)
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
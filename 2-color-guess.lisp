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
    (if (and (not (= *check* 0)) (not (= *check* 1)) (< *counter2* board)) (setf (nth *counter2* *guess*) (first *in-answer*)))) *guess*)
  ;;this is very similar to ab-color except that it checks for monochromatics because we do not know the initial colors in the answer

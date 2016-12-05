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
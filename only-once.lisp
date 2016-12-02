(defparameter *in-answer* ())
(defparameter *not-in-answer* ())
(defparameter *check* 0)
(defparameter *counter2* 0)
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
          (t (progn (incf *counter2*) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*)))))
  *guess*)
  ;;EXAMPLE: 5 pegs, 7 colors: answer is BAECD, in-answer: A B C D E, not-in-answer: F G, first guess is AFFFF -> returns (0,0), next guess is BFFFF -> returns (1,0) and delete B from in-answer, etc. until we get the answer
  ;;ANOTHER EXAMPLE: 5 pegs, 5 colors: answer is ABEDC, in-answer: A B C D E, not-in-answer is NOT used, first guess is AEEEE, we want the guess to return (2,0) (which it does), delete A from in-answer, next guess
  ;;is ABEEE, we want the guess to return (3,0) (which it does), delete B from in-answer, etc. bull-count is checked as being bull-count + 1 because the pivot element IS in the answer

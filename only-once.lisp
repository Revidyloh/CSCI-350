(defparameter *in-answer* ())
(defparameter *not-in-answer* ())
(defparameter *check* 0)
(defparameter *counter2* 0)
(defun only-once-guesser (board colors SCSA last-response)
  (declare (ignore SCSA)) ;;ignore SCSA 
  (when (equal last-response nil) (progn (setf *counter* 0) (setf *counter2* 0) (setf *check* 0) (setf *not-in-answer* ()) (setf *in-answer* ()) (setf *guess* ()) (setf *bull-check* 1))) ;;reset everything each round
  (when (and (= board (length colors)) (= *check* 0)) (progn (setf *check* 1) (setf *counter* (+ (length colors) 1)) (loop for i in colors do (push i *in-answer*)) (push 'ZZ *not-in-answer*))) ;;when board = colors, push all colors into in-answer and skip to ONLY-ONCE
  ;;MONOCHROMATIC GENERATION
  (when (and (equal last-response nil) (= *check* 0)) (setf *guess* (make-list board :initial-element 'A))) ;;last response is nil, make the guess all As
  (when (and (not (equal last-response nil)) (= *check* 0)) (setf *guess* (make-list board :initial-element (nth *counter* colors)))) ;;else it's not nil, we make the guess all Bs, Cs, etc.
  ;;END MONOCHROMATIC GENERATION
  (cond ((and (equal last-response nil) (= *check* 0)) (incf *counter*)) ;;with the very first guess (As), just ++ the counter
    ((= *check* 1) (setf *check* 1))
      ((and (= (first last-response) 0) (= *check* 0)) (progn (push (nth (- *counter* 1) colors) *not-in-answer*) (incf *counter*))) ;;if the previous guess yielded no bull count, add it to not-in-answer and ++ counter
        ((and (not (= (first last-response) 0)) (= *check* 0)) (progn (push (nth (- *counter* 1) colors) *in-answer*) (incf *counter*))) ;;else, the previous guess yielded a bull count, add it to in-answer and ++ counter
          (t (setf *check* 2))) ;;this is crucial so check doesn't change its value in order for the only-once part to run, IF CHECK = 2, WE ARE DOING ONLY-ONCE
  (when (and (> *counter* (length colors)) (not (= *check* 2))) (setf *check* 1)) ;;we are done with monochromatics, begin the first step of only-once
  (when (and (<= *counter* (length colors)) (not (= *check* 2))) (return-from only-once-guesser *guess*)) ;;we're still dealing with monochromatics, return the mono-guess and continue on
  ;;DONE WITH MONOCHROMATICS <--- this phase only begins once check is 1, when check is 2 we are in the process of only-once
  ;;;;BEGIN ONLY-ONCE
  (cond ((= *check* 1) (progn (setf *check* 2) (setf *guess* (make-list board :initial-element (first *not-in-answer*))) (setf *counter* 0) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*))) (print *guess*) (return-from only-once-guesser *guess*))
    ((= *bull-check* (first last-response)) (progn (delete (nth *counter2* *in-answer*) *in-answer*) (setf *counter2* 0) (incf *counter*) (incf *bull-check*) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*))))
        (t (progn (incf *counter2*) (setf (nth *counter* *guess*) (nth *counter2* *in-answer*))))) (print *guess*)
  *guess*)
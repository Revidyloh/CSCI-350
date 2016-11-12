(defun increment (colors-copy board)
  (if (= board 1) (loop for x in colors-copy collect (list x))
    (mapcan #'(lambda (x) (mapcar #'(lambda (y) (cons x y))
    (increment colors-copy (1- board)))) colors-copy)))

(setf counter2 0) ;;for monochromatic iteration
(setf counter3 0) ;;for increment
(setf in-answer '()) ;;store all colors in answers
(setf store-mono '()) ;;store monochromatics to be used with last-response
(defun baseline3 (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
  (let ((guess '()) (previous-guess '()) (check 0))
    (when (equal last-response nil) (progn (setf counter2 0) (setf counter3 0) (setf in-answer '()) (setf store-guess '()) (setf store-mono '()))) ;;reset everything after each round
    (if (= counter2 (length colors)) (setf check 1) ;;if we went through all monochromatics, skip the below and proceed onward
      (progn (when (equal last-response nil) (setf store-mono (loop for x in colors collect (make-list board :initial-element x)))) ;;generate all monochromatics, should only happen once
      (cond ((equal last-response nil) (progn (setf guess (nth counter2 store-mono)) (setf counter2 1))) ;;should only evaluate to true on first guess (all A's)
        (t (progn (setf guess (nth counter2 store-mono)) (setf previous-guess (nth (- counter2 1) store-mono)) (incf counter2))))
      (when (not (equal last-response nil)) (unless (= (first last-response) 0) (push (first previous-guess) in-answer))))
    )
    
    (when (and (equal (first (last colors)) (first (first (last store-mono)))) (= check 1)) (progn 
      (unless (= (first last-response) 0) (push (first (first (last store-mono))) in-answer)) (setf store-mono '()))) ;;evaluate the last monochromatic
      ;;should only run once per round, delete store-mono

    (if (= check 0) guess ;;should only evaluate to true for monochromatics
      (progn (setf guess (nth counter3 (increment in-answer board))) (incf counter3))) guess))
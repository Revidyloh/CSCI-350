;;;;;;;;;;;;;;;;;;;;;;;;;
(defun increment (colors-copy board)
  (if (= board 1) (loop for x in colors-copy collect (list x))
    (mapcan #'(lambda (x) (mapcar #'(lambda (y) (cons x y))
    (increment colors-copy (1- board)))) colors-copy)))

;;;;;;;;;;;;;;;; BASELINE1


(setf counter 0)
(defun baseline1 (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
  (let ((colors-copy colors) (guess '()))
    (if (equal last-response nil) (progn (setf counter 1) (setf guess (make-list board :initial-element 'A)))
      (progn (setf guess (nth counter (increment colors-copy board))) (incf counter)))
    guess))
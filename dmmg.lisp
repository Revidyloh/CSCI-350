(defparameter *counter* 0)
(defparameter *guess* ())
(defparameter *bull-check* 1)
(defun DMMG (board colors SCSA last-response)
  (when (equal SCSA 'ab-color) (ab-color-guesser board colors SCSA last-response))
  (when (equal SCSA 'only-once) (only-once-guesser board colors SCSA last-response))
  (when (equal SCSA 'first-and-last) (first-and-last-guesser board colors SCSA last-response))
  *guess*) 

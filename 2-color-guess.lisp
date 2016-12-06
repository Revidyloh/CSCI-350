(defparameter *counter* 0)
(defparameter *countermono* 0)
(defparameter *guess* ())
(defparameter *lastguess* ())
(defparameter *bull-check* 1)
(defparameter *foundcolors* ())



(defun 2-color-guesser (board colors SCSA last-response)





  (declare (ignore SCSA)) ;;ignore SCSA 



  	
  (when (equal last-response nil) (progn (setf *countermono* 0)(setf *foundcolors* ()) (setf *guess* ()) (setf *bull-check* 1)(setf *guess* (make-list board :initial-element (nth *countermono* colors)))
  	;(print *guess*)
  	(setf *lastguess* *guess*)
  	(setf *countermono* 1)
  	(return-from 2-color-guesser *guess*)	
  ))


  (when (and (> (first last-response)  0)(not (= (length *foundcolors*) 2))(not (equal last-response nil)))
  	(progn 
  		(print *lastguess*)
  		(print last-response)
  		
  		(setf *foundcolors* (cons (nth 1 *lastguess*) *foundcolors* ))

  		)

  	)



  		(when (not(= (length *foundcolors*) 2))

  			(progn 
  				;(print last-response)
  				
  				(setf *guess*(make-list board :initial-element (nth *countermono* colors)))
  					
  				;(print *guess*)

  				(setf *lastguess* *guess*)
  				(incf *countermono* 1)
  				)
  					(return-from 2-color-guesser *guess*)

  			)






  		(print *foundcolors*)


	;(when (= *foundcolor* 2) run Dans ab color code here)

	)


;mystery-1
;makes a list that alternates 3 colors
;(Mastermind 100 5 'three-color-alternating)
;(play-tournament *Mastermind* 'RandomFolks 'three-color-alternating 25)

(defun three-color-alternating (length colors)
  (loop with choices = (choose-n-random 3 colors) 
     
     with first-color = (first choices)
     with second-color = (second choices)
     with third-color =(third choices)
     
     for i from 1 to length 
     when (or (equal i 1) (equal (mod (+ i 2) 3)0)) 
     collect first-color
  	 when (or (equal (mod (- i 2)3) 0)(equal i 2))
     collect second-color
     when (equal (mod i 3) 0)
     collect third-color))
     
;there is a problem with this function becuase there is a chance that it could be only 2 color need to fix
;mystery-3
;makes a 3-color list 
;(Mastermind 7 5 'three-color) 
;(play-tournament *Mastermind* 'RandomFolks 'three-color 25)
(defun three-color (length colors)
  (loop with choices = (choose-n-random 3 colors)
     for i from 1 to length 
     collect (random-chooser choices)))
     
;mystery-4 
;makes a list that alternates 2 colors
;(Mastermind 7 5 'two-color-alternating)
;(play-tournament *Mastermind* 'RandomFolks 'two-color-alternating 25)
(defun two-color-alternating (length colors)
  (loop with choices = (choose-n-random 2 colors) 
     
     with first-color = (first choices)
     with second-color = (second choices)

     for i from 1 to length 
     when (oddp i) 
     collect first-color
     else collect second-color)) 
     
     
     
     
;mystery-2 and mystery-5 are working with probability i will try to find out their patterns 

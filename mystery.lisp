
;mystery-1 three-color-alternating
;makes a list that alternates 3 colors
;(Mastermind 100 5 'three-color-alternating)
;(play-tournament *Mastermind* 'RandomFolks 'three-color-alternating 25)


(defun mystery-1 (length colors)
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
(defun mystery-3 (length colors)
  (loop with choices = (choose-n-random 3 colors)
     for i from 1 to length 
     collect (random-chooser choices)))
     
;mystery-4 
;makes a list that alternates 2 colors
;(Mastermind 7 5 'two-color-alternating)
;(play-tournament *Mastermind* 'RandomFolks 'two-color-alternating 25)
(defun mystery-4 (length colors)
  (loop with choices = (choose-n-random 2 colors) 
     
     with first-color = (first choices)
     with second-color = (second choices)

     for i from 1 to length 
     when (oddp i) 
     collect first-color
     else collect second-color)) 
     
;mystery-5
;makes a list that usually has fewer (3 or 4) colors
;(Mastermind 7 5 'usually-fewer3or4)
;(play-tournament *Mastermind* 'RandomFolks 'usually-fewer3or4 25)
(defun mystery-5 (length colors)
  (let* ((probability (random 100))
	 (coin-flip (when (< probability 90) (random-chooser '(2 3))))
	 (choices (cond (coin-flip (if (= 3 coin-flip) (choose-n-random 4 colors) (choose-n-random 3 colors)))
			(t colors))))
    (loop for i from 1 to length
       collect (random-chooser choices))))


;Mystery-2
;makes a list with preferences for fewer colors
;(Mastermind 7 5 'prefer-fewer) 
;(play-tournament *Mastermind* 'RandomFolks 'prefer-fewer 25)

(defun mystery-2 (length colors)
  (let* ((probability (random 100))
	 (color-count (length colors))
	 (coin-flip (cond ((<= probability 49) 1)
		      ((<= probability 74) 2)
		      ((<= probability 87) (if (>= color-count 3) 3 color-count))
		      ((<= probability 95) (if (>= color-count 4) 4 color-count))
		      ((<= probability 98) (if (>= color-count 5) 5 color-count))
		      (t (if (>= color-count 6) (random-chooser (loop for i from 6 to color-count 
								   collect i)) color-count))))
	 (choices (choose-n-random coin-flip colors))) 
    (loop for i from 1 to length
       collect (random-chooser choices))))

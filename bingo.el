(require 'cl)

;; prototype, does not function yet
(defun play-board (board)
  (let ((game (make-game))
	(turns 0))
    (while (no-bingo board)
      (play-turn game board)
      (setq turns (1+ turns)))
    turns))

;; Functions related to the board object

(defmacro def-column (name start end)
  `(defvar ,name (number-sequence ,start ,end)))

(def-column *B* 1  15)
(def-column *I* 16 30)
(def-column *N* 31 45)
(def-column *G* 46 60)
(def-column *O* 61 75)

(defstruct board
  (b-col (rand-select 5 *B*))
  (i-col (rand-select 5 *I*))
  (n-col (rand-select 4 *N*))
  (g-col (rand-select 5 *G*))
  (o-col (rand-select 5 *O*)))

;; Game object

(defstruct game
  (numbers 
   (rand-permutation (number-sequence 1 75))))

(defun draw-number (game)
  (pop (game-numbers game)))
	    
;; Utilities

(defun remove-at (list index)
	 (cond
	  ((< index 0) (error "Invalid index"))
	  ((= index 0) (rest list))
	  (t           (cons (first list)
			     (remove-at (rest list) (1- index))))))

(defun rand-select (n list)
	 (cond
	  ((zerop n)   nil)
	  ((null list) nil)
	  (t           (let ((i (random (length list))))
			 (cons (nth i list)
			       (rand-select (1- n) (remove-at list i)))))))

(defun rand-permutation (list)
  (rand-select (length list) list))


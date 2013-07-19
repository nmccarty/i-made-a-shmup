(defpackage :imas.engine
  (:use :common-lisp
	:imas.bullets
	:imas.enemies))


;; Holds all the bullets currently in simulation in a list 
(defparameter *bullets* nil)
;; Holds all the bullets we have been asked to add to the simulation in a list
(defparameter *bullet-queue* nil)

;; Functions relating to bullets
(defun step-bullets ()
  "Advances the states of all the bullets by one tick.
   Deletes all bullets that are cullable."
  (setf *bullets*
	(remove-if #'bullet-cullable-p
		   *bullets*
		   :key #'bullet-step)))

(defun flush-bullets ()
  "Deletes all the bullets currently in the simulation"
  (setf *bullets* nil))

(defun add-bullet (bullet)
  "Adds bullet to the front of the bullet wait queue"
  (push bullet *bullet-queue*))


;; Holds all the enemys currently in the simulation in a list
(defparameter *enemies* nil)
;; Holds all the enemys we have been asked to add to the simulation in a list
(defparameter *enemy-queue* nil)

;; Functions relating to enemies
(defun step-enemies ()
  "Advances the states of all the enemies by one tick.
   Deletes all that are cullable."
  (setf *enemies*
	(remove-if #'enemy-cullable-p
		   *enemies*
		   :key #'enemy-step)))

(defun flush-enemies ()
  "Deletes all enemies currently in the simulation."
  (setf *enemies* nil))

(defun add-enemy (enemy)
  (push enemy *enemy-queue*))

(defun claim-all-bullets ()
  (mapcar #'add-bullet
	  (reduce #'append
		  (mapcar #'enemy-claim-bullets
			  *enemies*))))


;; Main engine stepping

;; Holds the age of the tick loop
(defparameter *tick-loop-age* 0)

;; Holds all the stepping functions
(defparameter *step-functions* (list #'step-bullets
				     #'(lambda ()
					 (loop while *bullet-queue* do
					   (push (pop *bullet-queue*) *bullets*)))
				     #'step-enemies
				     #'claim-all-bullets
				     #'(lambda ()
					 (loop while *enemy-queue* do
					      (push (pop *enemy-queue*) *enemies*)))
				     #'(lambda ()
					 (incf *tick-loop-age*))))

;; Main engine stepping function 
;; TODO add more stuff to this
(defun tick ()
  (dolist (f *step-functions*)
    (funcall f)))


;; Functions to aid in debugging
(defun advance-by-x-ticks (x)
  (declare (type fixnum x))
  (loop repeat x do
       (tick)))

(defun advance-by-10-ticks ()
  (advance-by-x-ticks 10))

(defun advance-by-120-ticks ()
  (advance-by-x-ticks 120))


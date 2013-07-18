(defpackage :imas.engine
  (:use :common-lisp :imas.bullets))


;; Holds all the bullets currently in simulation in a list 
(defparameter *bullets* nil)
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


;; Main engine stepping

;; Holds the age of the tick loop
(defparameter *tick-loop-age* 0)

;; Holds all the stepping functions
(defparameter *step-functions* (list #'step-bullets
				     #'(lambda ()
					 (while *bullet-queue*
					   (push (pop *bullet-queue*) *bullets*)))
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


(defpackage :imas.engine
  (:use :common-lisp :imas.bullets))


;; Holds all the bullets currently in simulation in a list 
(defparameter *bullets* nil)


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



;; Main engine stepping

;; Holds all the stepping functions
(defparameter *step-functions* (list #'step-bullets))

;; Main engine stepping function 
;; TODO add more stuff to this
(defun tick ()
  (dolist (f *step-functions*)
    (funcall f)))


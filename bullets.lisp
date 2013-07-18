(defpackage :imas.bullets
  (:use :common-lisp))


;; The base class that all bullet objects must inherit from
;; Bullets have three basic properties:
;; 1.) X position [-1.0<=x<=1.0]
;; 2.) Y position [-1.0<=y<=1.0]
;; 3.) Age in ticks [Intger value greter than 0]
(defclass bullet ()
  ((x-pos :accessor get-x-pos
	  :initarg :x-pos
	  :initform 0)
   (y-pos :accessor get-y-pos
	  :initarg :y-pos
	  :initform 0)
   (age :accessor get-age
	:initarg :age
	:initform 0)))


;; Interface that all bullets must have
;; step: Increases age by one tick and updates x-pos and y-pos accordingly
;; pos: returns a cons cell containing the x-pos and y-pos
;; collision-p: given hitcircle for player in terms of ((x-pos . y-pos) . radius), return t if the player has collided with the bullet
;; Cullable-p: returns t if the given bullet can safely be removed from the simulation now
(defgeneric bullet-step (bullet)
  (:documentation "Advances the state of bullet by one tick and returns bullet"))
(defgeneric bullet-pos (bullet)
  (:documentation "Returns position of the center of the bullet with form (x-pos . y-pos)"))
(defgeneric bullet-collision-p (bullet player-pos)
  (:documentation "Returns t if player described by player-pos of form ((x . y) . radius) has collided with bullet, nil if otherwise"))
(defgeneric bullet-cullable-p (bullet)
  (:documentation "Returns t if bullet can safely be deleted."))


;; Optional components of the bullet interface
;; radius: radius of the rough bounding circle of this bullet
;; velocity: rough current veclocity of form (speed . direction)
(defgeneric bullet-radius (bullet)
  (:documentation "Returns rough radius of bullet if avaible, returns 0 otherwise"))
(defgeneric bullet-velocity (bullet)
  (:documentation "Returns rough velocity of bullet with form (speed . direction) if avaible, returns (0 . 0) otherwise"))


;; Things that are implemented for you
;; get-pos: returns cons-cel containing the x-pos and y-pos
(defmethod bullet-pos ((bullet bullet))
  (cons (get-x-pos bullet) (get-y-pos bullet)))

(defmethod bullet-cullable-p ((bullet bullet))
  (or (> (get-x-pos bullet) 1)
      (> (get-y-pos bullet) 1)
      (< (get-x-pos bullet) -1)
      (< (get-y-pos bullet) -1)))


;; Required things that aren't implemented for you
;; All these throw errors if they get hit
(defmethod bullet-step ((bullet bullet))
  (error "Unimplemented required bullet method"))
(defmethod bullet-pos ((bullet bullet))
  (error "Unimplemented required bullet method"))
(defmethod bullet-collision-p ((bullet bullet) player-pos)
  (error "Unimplemented required bullet method"))


;; Things that aren't required or implmented for you
(defmethod bullet-radius ((bullet bullet)) 0)
(defmethod bullet-velocity ((bullet bullet)) (cons 0 0))


;; Print support for the bullet class
(defmethod print-object ((obj bullet) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (bullet-pos obj) stream)
    (princ #\Space stream)
    (princ (get-age obj) stream)))




;; A subclass of bullet that travels in straight lines and is a circle
;; Intended only for rapid prototyping and testing
;; Has radius in units, direction in radians, and speed in units per tick
(defclass circle-bullet (bullet)
  ((radius :accessor get-radius
	   :initarg :radius
	   :initform 0.01)
   (direction :accessor get-direction
	      :initarg :direction
	      :initform 0)
   (speed :accessor get-speed
	  :initarg :speed
	  :initform 1/120))
  (:documentation "A basic subclass of bullet intended for testing and rapid prototyping"))


;; Required interface elements for circlebullet
(defmethod bullet-step ((bullet circle-bullet))
  (let ((old-x     (get-x-pos bullet))
	(old-y     (get-y-pos bullet))
	(dir   (get-direction bullet))
	(speed     (get-speed bullet)))
    (incf (get-age bullet))
    (setf (get-x-pos bullet) (+ old-x (* speed (cos dir))))
    (setf (get-y-pos bullet) (+ old-y (* speed (sin dir)))))
  bullet)

(defmethod  bullet-pos ((bullet circle-bullet))
  (cons (get-x-pos bullet) (get-y-pos bullet)))

(defmethod bullet-collision-p ((bullet circle-bullet) player-pos)
  (< (+ (expt (- (get-x-pos bullet)
		 (caar player-pos))
	      2)
	(expt (- (get-y-pos bullet)
		 (cdar player-pos))
	      2))
     (expt (cdr player-pos) 2)))


;; Optional interface elements for circlebullet
(defmethod bullet-radius ((bullet circle-bullet))
  (get-radius bullet))

(defmethod bullet-velocity ((bullet circle-bullet))
  (cons (get-speed bullet) (get-direction bullet)))




;; A more useful implementation of bullet
(defclass parametric-bullet (bullet)
  ((x-function :accessor get-x-function
	       :initarg :x-func
	       :initform #'(lambda (x) (1- (/ x 120))))
   (y-function :accessor get-y-function
	       :initarg :y-func
	       :initform #'(lambda (y) (1- (/ y 120))))
   (radius :accessor get-radius
	   :initarg :radius
	   :initform 0.01))
  (:documentation "A subclass of bullet that describes position as a parametric function."))


;; Required interface elements for parametric-bullet
(defmethod bullet-step ((bullet parametric-bullet))
  (incf (get-age bullet))
  (setf (get-x-pos bullet)
	(funcall (get-x-function bullet) (get-age bullet)))
  (setf (get-y-pos bullet)
	(funcall (get-y-function bullet) (get-age bullet))))

(defmethod bullet-collision-p ((bullet parametric-bullet) player-pos)
  (< (+ (expt (- (get-x-pos bullet)
		 (caar player-pos))
	      2)
	(expt (- (get-y-pos bullet)
		 (cdar player-pos))
	      2))
     (expt (cdr player-pos) 2)))


;; Optional interface elements for parametric bullets
(defmethod bullet-radius ((bullet parametric-bullet))
  (get-radius bullet))

(defmethod bullet-velocity ((bullet parametric-bullet))
  (let ((curr-x (get-x-pos bullet))
	(curr-y (get-y-pos bullet))
	(next-x (funcall (get-x-function bullet)
			 (1+ (get-age bullet))))
	(next-y (funcall (get-y-function bullet)
			 (1+ (get-age bullet)))))
    (error "You haven't implemented this yet for parametric bullet")))

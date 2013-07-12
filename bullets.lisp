(defpackage :imas.bullets
  (:use :common-lisp))


;; The base class that all bullet objects must inherit from
;; Bullets have three basic properties:
;; 1.) X position [-1.0<=x<=1.0]
;; 2.) Y position [-1.0<=y<=1.0]
;; 3.) Age in ticks [Intger value greater than 0]
(defclass bullet ()
  ((x-pos :accessor get-x-pos
	  :initarg :x-pos
	  :initform 0)
   (y-pos :accessor get-y-pos
	  :initarg :y-pos
	  :initform 0)
   (age :accessor get-ages
	:initarg :age
	:initform 0)))


;; Interface that all bullets must have
;; step: Increases age by one tick and updates x-pos and y-pos accordingly
;; get-pos: returns a cons cell containing the x-pos and y-pos
;; collision-p: given hitcircle for player in terms of ((x-pos . y-pos) . radius), return t if the player has collided with the bullet
(defgeneric step-bullet (bullet))
(defgeneric bullet-pos (bullet))
(defgeneric bullet-collision-p (bullet player-pos))

;; Optional components of the bullet interface
;; radius: radius of the rough bounding circle of this bullet
(defgeneric bullet-radius (bullet))


;; Things that are implemented for you
;; get-pos: returns cons-cel containing the x-pos and y-pos
(defmethod bullet-pos ((bullet bullet))
  (cons (get-x-pos bullet) (get-y-pos bullet)))

;; Required things that aren't implemented for you
;; All these throw errors if they get hit
(defmethod step-bullet ((bullet bullet))
  (error "Unimplemented required bullet method"))
(defmethod get-pos ((bullet bullet))
  (error "Unimplemented required bullet method"))
(defmethod bullet-collision-p ((bullet bullet))
  (error "Unimplemented required bullet method"))

;; Things that aren't required or implmented for you
(defmethod bullet-radius ((bullet bullet)) 0)

(defpackage :imas.enemies
  (:use :common-lisp :imas.bullets))

;; Basic class that all enemies extend from 
(defclass enemy
    ((x-pos :accessor get-x-pos
	    :initarg :x-pos
	    :initform 0)
     (y-pos :accessor get-y-pos
	    :initarg :y-pos
	    :initform 0)
     (age :accessor get-age
	  :initarg :age
	  :initform 0)
     (health :accessor get-health
	     :initarg :health
	     :initform 1.0d0)
     (unclaimed-bullets :accessor get-unclaimed-bullets
			:initarg :bullets
			:initform nil)))


;; Required interface elements for enemy
;; step: Increases age by one tick and updates enemy state accordingly
(defgeneric enemy-step (enemy)
  (:docmentation "Advances state of enemy by one tick and returns enemy."))
;; pos: returns a cons cell containing the position of the center of the enemy
(defgeneric enemy-pos (enemy)
  (:documentation "Returns current position of enemy in form (x-pos . y-pos)."))
;; collision-p: returns t if player described by player-pos of form ((x . y) . radius)
;; has collided with enemy
(defgeneric enemy-collision-p (enemy pos-pair)
  (:documentation "Returns t if thing described by pos-pair has collided with enemy."))
;; cullable-p: returns t if the enemy is either dead or is no longer relevant
(defgeneric enemy-cullable-p (enemy)
  (:documentation "Returns t if enemy can safely be removed from the simulation."))
;; claim-bullets: claims this ememy's unclamimed bullets
(defgeneric enemy-claim-bullets (enemy)
  (:documentation "Returns a list containing all of the bullets in this enemy's unclaimed bullet queue and flush it."))
;; max-health: the maximium ammount of health this enemy can have in any arbitrary format
(defgeneric enemy-max-health (enemy)
  (:documentation "The maximium health this enemy can have."))
;; examine-health: Returns the health of this enemy as the ratio between its max health and current health
(defgeneric enemy-examine-health (enemy)
  (:documentation "Returns the health of this enemy as ratio between max and current health."))
;; damage: Deals ammount damage to enemy and updates state of enemy to reflect this 
(defgeneric enemy-damage (enemy ammount)
  (:documentation "Damamges enemy by ammount."))

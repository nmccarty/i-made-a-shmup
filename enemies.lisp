(defpackage :imas.enemies
  (:use :common-lisp :imas.bullets))

;; Basic class that all enemies extend from 
(defclass enemy ()
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
     (max-health :accessor get-max-health
		 :initarg :max-health
		 :initform 100)
     (unclaimed-bullets :accessor get-unclaimed-bullets
			:initarg :bullets
			:initform nil)))


;; Required interface elements for enemy
;; step: Increases age by one tick and updates enemy state accordingly
(defgeneric enemy-step (enemy)
  (:documentation "Advances state of enemy by one tick and returns enemy."))
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
;; real-health: Returns the actual ammount of health the enemy has
(defgeneric enemy-real-health (enemy)
  (:documentation "Returns the actual ammount of health enemy has, usually enemy-examine-health * enemy-max-health."))
;; damage: Deals ammount damage to enemy and updates state of enemy to reflect this 
(defgeneric enemy-damage (enemy ammount)
  (:documentation "Damamges enemy by ammount."))


;; Parts of the required interface that are implemented for you
;; Simply conses the x-pos and y-pos, add your own method if you need something fancier
(defmethod enemy-pos (enemy)
  (cons (get-x-pos enemy) (get-y-pos enemy)))

;; Simply returns the list in slot unclaimed bullets and flushes it
(defmethod enemy-claim-bullets (enemy)
  (let (list (get-unclaimed-bullets enemy))
    (setf (get-unclaimed-bullets enemy) nil)
    list))

;; Simply returns what is in slot max-health
(defmethod enemy-max-health (enemy)
  (get-max-health enemy))

;; Simply returns what is in slot health
(defmethod enemy-examine-health (enemy)
  (get-health enemy))

;; Returns health * max-health
(defmethod enemy-real-health (enemy)
  (* (get-max-health enemy)
     (get-health enemy)))

;; Checks if the enemy is signifigantly out of bounds or has a health <= 0
(defmethod enemy-cullable-p (enemy)
  (or (<= (enemy-examine-health enemy) 0)
      (> (get-x-pos enemy) 1.1)
      (> (get-y-pos enemy) 1.1)
      (< (get-x-pos enemy) -1.1)
      (< (get-x-pos enemy) -1.1)))


;; Parts of the required interface that aren't implemented for you
(defmethod enemy-collision-p (enemy)
  (error (concatenate 'string
		      "Enemy-collision-p not defined for type:"
		      (prin1-to-string (type-of enemy)))))
(defmethod enemy-damage-p (enemy)
  (error (concatenate 'string
		      "Enemy-damage-p not defined for type:"
		      (prin1-to-string (type-of enemy)))))

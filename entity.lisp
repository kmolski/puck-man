(defparameter *base-speed* 1)
(defparameter *default-respawn-time* 30)

(defclass game-entity ()
  ((position :initarg :position
             :initform (error "no value for slot 'position'")
             :reader entity-position
             :documentation "The position (x . y) of the entity")
   (direction :initarg :direction
              :initform (error "no value for slot 'direction'")
              :reader entity-direction
              :documentation "The direction that the entity is facing")
   (speed :initform *base-speed*
          :reader entity-speed
          :documentation "The speed of the entity")
   (game-state :initarg :game-state
               :reader entity-game
               :documentation "The game that the entity belongs to"))
  (:documentation "Represents a basic game entity"))

(defgeneric can-traverse-tile-p (entity tile)
  (:documentation "Checks if the entity can enter the map tile"))

(defgeneric check-collision (entity)
  (:documentation "Checks if there is a collision between the player and ghost"))

(defgeneric move (entity)
  (:documentation "Move the entity to a new position based on its speed and direction"))

(defgeneric draw (entity renderer)
  (:documentation "Draw the entity with the given renderer."))

(defparameter *move-step* (/ 1.0 32))

(defmethod can-move-p ((entity game-entity) positions-and-tiles)
  "Returns T if the entity can make the move described by positions-and-tiles."
  (every (lambda (pos-and-tile) (can-traverse-tile-p entity (cdr pos-and-tile)))
         positions-and-tiles))

(defun get-move-direction (from-pos to-pos)
  "Returns the direction of the move from-pos -> to-pos."
  (append (if (< (car from-pos) (car to-pos)) '(right) '(left))
          (if (< (cdr from-pos) (cdr to-pos)) '(down)  '(up))))

(define-condition entity-collision () ())

(defmethod move-and-check-collision ((entity game-entity))
  "Move the entity and execute the check-collision method."
  (with-slots (direction position speed) entity
      (let* ((game (entity-game entity))
             (map (game-map game)))
        (loop for i below (entity-speed entity)
              for prev-position = (copy-list position)
              for next-tiles = (get-next-tiles map position direction *move-step*)
              when (can-move-p entity next-tiles)
                do (move-to-next-tile entity direction *move-step*)
                   (loop for pos-and-tile in next-tiles
                         for new-pos = (car pos-and-tile)
                         for tile = (cdr pos-and-tile)
                         when (and (member direction (get-move-direction prev-position new-pos)) (portal-p tile))
                           do (setf position (copy-list (get-other-portal-pos map new-pos)))
                              (move-to-next-tile entity direction *move-step*))
                   (check-collision entity)))))

(defmethod move-to-next-tile ((entity game-entity) direction step)
  "Move the entity to the next tile in the given direction."
  (with-slots (position) entity
    (case direction
      (up    (decf (cdr position) step))
      (left  (decf (car position) step))
      (right (incf (car position) step))
      (down  (incf (cdr position) step)))))

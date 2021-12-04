(require :sdl2)

(defclass player (game-entity)
  ((position :documentation "Actual position (x . y) of the player")
   (direction :initform 'none
              :documentation "The direction that the player is facing")
   (next-direction :initform 'none
                   :reader player-next-dir
                   :documentation "Direction advice for the player")
   (apparent-position :reader player-position
                      :documentation "Player's apparent position (x . y)"))
  (:documentation "Represents the player entity"))

(defmethod initialize-instance :after ((player player) &rest rest)
  "Set the player's apparent position to their real position."
  (declare (ignore rest))
  (with-slots (apparent-position position) player
    (setf apparent-position (copy-list position))))

(defmethod set-next-dir ((player player) keycode)
  "Set the player's next direction based on the player's input."
  (with-slots (next-direction) player
    (cond ((sdl2:scancode= keycode :scancode-up)    (setf next-direction 'up))
          ((sdl2:scancode= keycode :scancode-left)  (setf next-direction 'left))
          ((sdl2:scancode= keycode :scancode-down)  (setf next-direction 'down))
          ((sdl2:scancode= keycode :scancode-right) (setf next-direction 'right)))))

(defmethod can-traverse-tile-p ((player player) tile)
  "Returns T if the player can traverse the tile."
  (and (member tile '(empty portal-A portal-B portal-C portal-D
                      dot super-dot spawn-gate player-spawn))
       t))

(defun round-position (position)
  "Round the position to the nearest integral coordinates."
  (destructuring-bind (x . y) position
    (cons (round x) (round y))))

(defmethod check-collision ((player player))
  "Check if the player has collected a dot."
  (with-slots (game-state position) player
      (let* ((map (game-map game-state))
             (rounded-pos (round-position position)))
        (when (tile-exists-p map rounded-pos)
          (case (tile-at map rounded-pos)
            (dot (progn (set-tile-at map rounded-pos 'empty)
                        (incf (game-score game-state) 10)
                        (decf (game-dots game-state))))
            (super-dot (progn (set-tile-at map rounded-pos 'empty)
                              (incf (game-score game-state) 100))))))))

(defmethod move-and-check-collision :before ((player player))
  "Set the player's apparent position and change their direction if possible."
  (with-slots (apparent-position direction game-state next-direction position speed) player
    (setf apparent-position (copy-list position))
    (let ((map (game-map game-state)))
      (when (can-move-p player (get-next-tiles map position next-direction *move-step*))
        (setf direction next-direction)))))

(defparameter *player-sprites*
  (concatenate 'vector
               (make-sprite-vector +spritemap-entity-size+ 0 168 1)
               (make-sprite-vector +spritemap-entity-size+ 0 72 8)))

(defun select-player-sprite (direction time)
  "Select the player's sprite based on their direction and time."
  (let ((time-offset (if (evenp time) 0 2))
        (direction-offset (ecase direction ((none left) 1) (up 2) (down 6) (right 5))))
    (elt *player-sprites* (* (+ direction-offset time-offset) (if (eq direction 'none) 0 1)))))

(defmethod draw ((player player) renderer)
  "Draw the player with the given renderer."
  (let ((dest (sdl2:make-rect (round (+ (car *draw-start*) (* (car (entity-position player)) *tile-edge*)))
                              (round (+ (cdr *draw-start*) (* (cdr (entity-position player)) *tile-edge*)))
                              *tile-edge* *tile-edge*))
        (direction (entity-direction player))
        (time (get-universal-time)))
    (sdl2:render-copy renderer *spritemap-texture*
                      :source-rect (select-player-sprite direction time)
                      :dest-rect dest)))

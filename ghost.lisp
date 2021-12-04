(require :sdl2)

(defclass ghost (game-entity)
  ((direction :initform 'left
              :documentation "The direction that the ghost is facing")
   (alive :initform nil
          :accessor ghost-alive
          :documentation "T if the ghost is alive")
   (time-to-respawn :reader ghost-time-to-respawn
                    :documentation "Ghost respawn time")
   (tracking-strategy :accessor ghost-strategy
                      :documentation "Ghost's tracking strategy")))

(defmethod initialize-instance :after ((ghost ghost) &rest rest &key index &allow-other-keys)
  (declare (ignore rest))
  (with-slots (time-to-respawn tracking-strategy) ghost
    (setf time-to-respawn (* index *default-respawn-time*))))

(defmethod can-traverse-tile-p ((ghost ghost) tile)
  (and (member tile '(empty portal-A portal-B portal-C portal-D
                      dot super-dot spawn-gate player-spawn))
       t))

(defmethod check-collision ((ghost ghost))
  (with-slots (game-state position) ghost
    (when (< (get-squared-distance position (entity-position (game-player game-state))) 0.5)
      (signal 'entity-collision))))

(defmethod move-and-check-collision :before ((ghost ghost))
  (with-slots (alive direction game-state position speed time-to-respawn tracking-strategy) ghost
    (when (and (not alive) (> (game-duration game-state) time-to-respawn))
      (setf alive t)
      (setf position (copy-list (ghost-spawn-gate (game-map game-state))))
      (setf speed 1))
    (setf direction (get-move tracking-strategy))))

(defparameter *blinky-sprites* (make-sprite-vector +spritemap-entity-size+ 0 144 8)) ;; follow
(defparameter *inky-sprites* (make-sprite-vector +spritemap-entity-size+ 192 192 8)) ;; random
(defparameter *pinky-sprites* (make-sprite-vector +spritemap-entity-size+ 0 192 8))  ;; ambush
(defparameter *clyde-sprites* (make-sprite-vector +spritemap-entity-size+ 0 216 8)) ;; patrol

(defun select-ghost-sprite (sprite-vector direction time)
  (let ((time-offset (if (evenp time) 0 1))
        (direction-offset (ecase direction
                            ((none left) 4)
                            (up          6)
                            (down        2)
                            (right       0))))
    (elt sprite-vector (+ direction-offset time-offset))))

(defmethod draw ((ghost ghost) renderer)
  "Draw the ghost with the given renderer."
  (let* ((ghost-strategy (ghost-strategy ghost))
         (dest (sdl2:make-rect (round (+ (car *draw-start*) (* (car (entity-position ghost)) *tile-edge*)))
                               (round (+ (cdr *draw-start*) (* (cdr (entity-position ghost)) *tile-edge*)))
                               *tile-edge* *tile-edge*))
         (direction (entity-direction ghost))
         (time (get-universal-time))
         (sprite-vec (etypecase ghost-strategy
                       (track-follow *blinky-sprites*)
                       (track-patrol *clyde-sprites*)
                       (track-ambush *pinky-sprites*)
                       (track-random *inky-sprites*))))
    (sdl2:render-copy renderer *spritemap-texture*
                      :source-rect (select-ghost-sprite sprite-vec direction time)
                      :dest-rect dest)))

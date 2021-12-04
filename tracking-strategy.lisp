(defun get-squared-distance (pos1 pos2)
  "Get the squared distance between pos1 and pos2."
  (+ (expt (- (car pos1) (car pos2)) 2)
     (expt (- (cdr pos1) (cdr pos2)) 2)))

(defun get-opposite-direction (direction)
  "Return the opposite direction."
  (ecase direction
    (up    'down)
    (left  'right)
    (down  'up)
    (right 'left)
    (none  'none)))

(defclass tracking-strategy ()
  ((owner :initarg :owner
          :initform (error "no value for slot 'owner'")
          :documentation "The ghost object that owns the strategy")
   (next-direction :initform 'left
                   :documentation "Direction of the ghost's next move"))
  (:documentation "Represents the tracking strategy of a ghost"))

(defgeneric get-target (tracking-strategy)
  (:documentation "Get the target that will be chased using the strategy"))

(defmethod get-next-dir ((strategy tracking-strategy) choices)
  "Select the next direction for the ghost, based on available choices."
  (with-slots (next-direction) strategy
    (if choices
        (car (reduce (lambda (a b) (if (< (cdr a) (cdr b)) a b)) choices))
        next-direction)))

(defun get-possible-moves (current-direction current-position map owner target-position)
  "Get all possible moves for the ghost."
  (loop for direction in '(up left down right)
        for next-position = (get-next-pos current-position direction *move-step*)
        for next-tiles = (get-next-tiles map current-position direction *move-step*)
        when (and (not (eq current-direction (get-opposite-direction direction)))
                  (can-move-p owner next-tiles))
          collect (cons direction (get-squared-distance target-position next-position))))

(defmethod get-move ((strategy tracking-strategy))
  "Get the next move for the ghost."
  (with-slots (next-direction owner) strategy
    (let* ((current-direction next-direction)
           (target-position (get-target strategy))
           (map (game-map (entity-game owner)))
           (owner-position (entity-position owner))
           (current-position
             (if (tile-exists-p map (get-next-pos owner-position current-direction *move-step*))
                 (get-next-pos owner-position current-direction *move-step*)
                 owner-position))
           (choices (get-possible-moves current-direction current-position map owner target-position)))
      (setf next-direction (get-next-dir strategy choices))
      current-direction)))

(defclass track-follow (tracking-strategy) ()
  (:documentation "The basic following strategy for a ghost"))

(defmethod get-target ((strategy track-follow))
  "Return the player's apparent position."
  (with-slots (owner) strategy
    (player-position (game-player (entity-game owner)))))

(defclass track-patrol (tracking-strategy)
  ((center :documentation "Position of the map center")
   (radius :documentation "Max radius from the map center"))
  (:documentation "Area patrol strategy for the ghost"))

(defmethod initialize-instance :after ((strategy track-patrol) &rest rest)
  (declare (ignore rest))
  (with-slots (center owner radius) strategy
    (let* ((map (game-map (entity-game owner)))
           (map-dimensions (array-dimensions (map-tiles map)))
           (map-width  (first map-dimensions))
           (map-height (second map-dimensions)))
    (setf center (cons (random map-width) (random map-height)))
    (setf radius (max map-width map-height)))))

(defmethod get-target ((strategy track-patrol))
  "Return the point on the map that is the center of the ghost's patrol area."
  (slot-value strategy 'center))

(defmethod get-next-dir ((strategy track-patrol) choices)
  (with-slots (next-direction owner radius) strategy
    (if choices
        (let ((in-radius (remove-if (lambda (x) (> (cdr x) (expt radius 2))) choices)))
          (car (if in-radius
                   (nth (random (length in-radius)) in-radius)
                   (reduce (lambda (a b) (if (< (cdr a) (cdr b)) a b)) choices))))
        next-direction)))

(defparameter *ambush-max-lookahead* 5)
(defparameter *ambush-switch-to-follow-range* 5.0)

(defclass track-ambush (tracking-strategy) ()
  (:documentation "Ambush strategy for the ghost - chase the player's future position"))

(defmethod get-target ((strategy track-ambush))
  "Predict the player's future position."
  (with-slots (next-direction owner) strategy
    (let* ((game (entity-game owner))
           (player (game-player game))
           (map (game-map game))
           (player-direction (entity-direction player))
           (player-position (copy-list (player-position player)))
           (owner-position (entity-position owner))
           (current-position
             (if (tile-exists-p map (get-next-pos owner-position next-direction *move-step*))
                 (get-next-pos owner-position next-direction *move-step*)
                 owner-position))
           (distance (sqrt (get-squared-distance player-position current-position))))
      (when (> distance *ambush-switch-to-follow-range*)
        (loop for i below *ambush-max-lookahead*
              for next-player-positions = (get-next-tiles map player-position player-direction *move-step*)
              if (can-move-p player next-player-positions)
                do (setf player-position (get-next-pos player-position player-direction *move-step*))
              else
                do (return)))
      player-position)))

(defparameter *random-time-between-strat-changes* 10.0)

(defclass track-random (tracking-strategy)
  ((current-strategy :documentation "The current tracking strategy")
   (timer :initform (get-universal-time)
          :documentation "Time of the last strategy switch"))
  (:documentation "Random strategy - switches between follow, ambush and patrol"))

(defmethod initialize-instance :after ((strategy track-random) &rest rest &key owner &allow-other-keys)
  "Set an initial random strategy."
  (declare (ignore rest))
  (with-slots (current-strategy) strategy
    (setf current-strategy (get-random-strategy owner 3))))

(defmethod get-move :before ((strategy track-random))
  "Change the random strategy if enough time has passed."
  (with-slots (current-strategy owner timer) strategy
    (when (> (- timer (get-universal-time)) *random-time-between-strat-changes*)
      (setf current-strategy (get-random-strategy owner 3)))))

(defmethod get-target ((strategy track-random))
  (with-slots (current-strategy) strategy
    (get-target current-strategy)))

(defmethod get-next-dir ((strategy track-random) choices)
  (with-slots (current-strategy) strategy
    (get-next-dir current-strategy choices)))

(require :alexandria)
(require :sdl2)
(require :str)
(require :trivia)

(setf *random-state* (make-random-state t))

;; entity stuff

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

(defun get-squared-distance (pos1 pos2)
  (+ (expt (- (car pos1) (car pos2)) 2)
     (expt (- (cdr pos1) (cdr pos2)) 2)))

(defun get-opposite-direction (direction)
  (ecase direction
    (up    'down)
    (left  'right)
    (down  'up)
    (right 'left)
    (none  'none)))

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

(defmethod move-and-check-collision ((entity game-entity))
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
                   (when (check-collision entity)
                     nil) ;; TODO: signal collision
                   ))))

(defmethod move-to-next-tile ((entity game-entity) direction step)
  (with-slots (position) entity
    (format t "Position before: ~A ~A, direction ~A~%" entity position direction)
    (case direction
      (up    (decf (cdr position) step))
      (left  (decf (car position) step))
      (right (incf (car position) step))
      (down  (incf (cdr position) step)))
    (format t "Position after: ~A ~A ~%" entity position)))

(defclass ghost (game-entity)
  ((direction :initform 'left
              :documentation "The direction that the ghost is facing")
   (alive :initform nil
          :accessor ghost-alive
          :documentation "T if the ghost is alive")
   (time-to-respawn :reader ghost-time-to-respawn
                    :documentation "Ghost respawn time")
   (tracking-strategy :accessor ghost-strategy
                      :documentation "Ghost's tracking strategy")
   (ability :initform nil
            :accessor ghost-ability
            :documentation "Ghost's special ability")))

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
    (equal position (entity-position (game-player game-state)))))

(defmethod move-and-check-collision :before ((ghost ghost))
  (with-slots (ability alive direction game-state position speed time-to-respawn tracking-strategy) ghost
    (when (and (not alive) (> (game-duration game-state) time-to-respawn))
      (setf alive t)
      (setf position (copy-list (ghost-spawn-gate (game-map game-state))))
      (setf speed 1))
    (when (and alive ability)
      (ability-before-move ability))
    (setf direction (get-move tracking-strategy))))

(defmethod move-and-check-collision :after ((ghost ghost))
  (with-slots (ability alive) ghost
      (when (and alive ability)
        (ability-after-move ability))))

(defmethod draw ((ghost ghost) renderer)
  "Draw the ghost with the given renderer."
  (let* ((ghost-strategy (ghost-strategy ghost))
         (rect (sdl2:make-rect (round (+ (car *draw-start*) (* (car (entity-position ghost)) *tile-edge*)))
                               (round (+ (cdr *draw-start*) (* (cdr (entity-position ghost)) *tile-edge*)))
                               *tile-edge* *tile-edge*))
         (color (etypecase ghost-strategy
                  (track-follow '(192 0 255 255))
                  (track-patrol '(0 192 255 255))
                  (track-ambush '(192 192 0 255))
                  (track-random '(0 255 0 255)))))
    (apply #'sdl2:set-render-draw-color renderer color)
    (sdl2:render-fill-rect renderer rect)))

(defclass tracking-strategy ()
  ((owner :initarg :owner
          :initform (error "no value for slot 'owner'")
          :documentation "The ghost object that owns the strategy")
   (next-direction :initform 'left
                   :documentation "Direction of the ghost's next move")))

(defgeneric get-target (tracking-strategy))

(defmethod get-next-dir ((strategy tracking-strategy) choices)
  (with-slots (next-direction) strategy
    (if choices
        (car (reduce (lambda (a b) (if (< (cdr a) (cdr b)) a b)) choices))
        next-direction)))

(defun get-possible-moves (current-direction current-position map owner target-position)
  (loop for direction in '(up left down right)
        for next-position = (get-next-pos current-position direction *move-step*)
        for next-tiles = (get-next-tiles map current-position direction *move-step*)
        when (and (not (eq current-direction (get-opposite-direction direction)))
                  (can-move-p owner next-tiles))
          collect (cons direction (get-squared-distance target-position next-position))))

(defmethod get-move ((strategy tracking-strategy))
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

(defclass track-follow (tracking-strategy) ())

(defmethod get-target ((strategy track-follow))
  (with-slots (owner) strategy
    (player-position (game-player (entity-game owner)))))

(defclass track-patrol (tracking-strategy)
  ((center :documentation "Position of the map center")
   (radius :documentation "Max radius from the map center")))

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
  (slot-value strategy 'center))

(defmethod get-next-dir ((strategy track-patrol) choices)
  (with-slots (next-direction owner radius) strategy
    (if choices
        (let ((in-radius (remove-if (lambda (x) (> (cdr x) (expt radius 2))) choices)))
          (car (if in-radius
                   (nth (random (length in-radius)) in-radius)
                   (reduce #'min choices :key #'cdr))))
        next-direction)))

(defparameter *ambush-max-lookahead* 5)
(defparameter *ambush-switch-to-follow-range* 5.0)

(defclass track-ambush (tracking-strategy) ())

(defmethod get-target ((strategy track-ambush))
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
          :documentation "Time of the last strategy switch")))

(defmethod initialize-instance :after ((strategy track-random) &rest rest &key owner &allow-other-keys)
  (declare (ignore rest))
  (with-slots (current-strategy) strategy
    (setf current-strategy (get-random-strategy owner 3))))

(defmethod get-move :before ((strategy track-random))
  (with-slots (current-strategy owner timer) strategy
    (when (> (- timer (get-universal-time)) *random-time-between-strat-changes*)
      (setf current-strategy (get-random-strategy owner 3)))))

(defmethod get-target ((strategy track-random))
  (with-slots (current-strategy) strategy
    (get-target current-strategy)))

(defmethod get-next-dir ((strategy track-random) choices)
  (with-slots (current-strategy) strategy
    (get-next-dir current-strategy choices)))

(defclass player (game-entity)
  ((ability :initform nil
            :documentation "The player's ability")
   (position :documentation "Actual position (x . y) of the player")
   (direction :initform 'none
              :documentation "The direction that the player is facing")
   (next-direction :initform 'none
                   :reader player-next-dir
                   :documentation "Direction advice for the player")
   (apparent-position :reader player-position
                      :documentation "Player's apparent position (x . y)")))

(defmethod initialize-instance :after ((player player) &rest rest)
  (declare (ignore rest))
  (with-slots (apparent-position position) player
    (setf apparent-position (copy-list position))))

(defmethod set-next-dir ((player player) keycode)
  (with-slots (next-direction) player
    (cond ((sdl2:scancode= keycode :scancode-up)    (setf next-direction 'up))
          ((sdl2:scancode= keycode :scancode-left)  (setf next-direction 'left))
          ((sdl2:scancode= keycode :scancode-down)  (setf next-direction 'down))
          ((sdl2:scancode= keycode :scancode-right) (setf next-direction 'right)))))

(defmethod can-traverse-tile-p ((player player) tile)
  (and (member tile '(empty portal-A portal-B portal-C portal-D
                      dot super-dot spawn-gate player-spawn))
       t))

(defun round-position (position)
  (destructuring-bind (x . y) position
    (cons (round x) (round y))))

(defmethod check-collision ((player player))
  (with-slots (ability game-state position) player
      (let* ((map (game-map game-state))
             (rounded-pos (round-position position)))
        (when (tile-exists-p map rounded-pos)
          (case (tile-at map rounded-pos)
            (dot (progn (set-tile-at map rounded-pos 'empty)
                        (incf (game-score game-state) 10)
                        (decf (game-dots game-state))))
            (super-dot (progn (set-tile-at map rounded-pos 'empty)
                              (incf (game-score game-state) 100)
                              (setf ability (get-random-player-ability)))))))))

(defmethod move-and-check-collision :before ((player player))
  (with-slots (ability apparent-position direction game-state next-direction position speed) player
    (setf apparent-position (copy-list position))
    (let ((map (game-map game-state)))
      (when (can-move-p player (get-next-tiles map position next-direction *move-step*))
        (setf direction next-direction)))
    (when ability
      (if (> (ability-active-time ability) (ability-duration ability))
          (ability-reset ability)
          (ability-before-move ability)))))

(defmethod move-and-check-collision :after ((player player))
  (with-slots (ability) player
      (when ability
        (ability-after-move ability))))

(defmethod draw ((player player) renderer)
  "Draw the player with the given renderer."
  (let ((rect (sdl2:make-rect (round (+ (car *draw-start*) (* (car (entity-position player)) *tile-edge*)))
                              (round (+ (cdr *draw-start*) (* (cdr (entity-position player)) *tile-edge*)))
                              *tile-edge* *tile-edge*)))
    (sdl2:set-render-draw-color renderer 255 144 0 255)
    (sdl2:render-fill-rect renderer rect)))

;; game model stuff

(defparameter *time-to-start* 10)
(defparameter *default-lives* 3)

(defclass game-state ()
  ((map :initarg :map
        :reader game-map
        :initform (error "no value for slot 'map'"))
   (player :initarg :player
           :reader game-player
           :initform (error "no value for slot 'player'"))
   (ghosts :reader game-ghosts)
   (window :initarg window
           ; :initform (error "no value for slot 'window'"))
           )
   (level :initform 1)
   (score :accessor game-score
          :initform 0)
   (dots :accessor game-dots
         :initform 0)
   (lives :initform *default-lives*)
   (timer-start)
   (time-at-pause)
   (stage :initform 'init)))

(defmethod initialize-instance :after ((game game-state) &rest rest)
  (declare (ignore rest))
  (with-slots (game-state) (game-player game)
    (setf game-state game)))

(defun get-random-strategy (owner &optional (max-index 4))
  (let ((class (ecase (random max-index)
                 (0 'track-follow)
                 (1 'track-ambush)
                 (2 'track-patrol)
                 (3 'track-random))))
    (make-instance class :owner owner)))

(defun get-random-ghost-ability ()
  ())

(defun get-random-player-ability ()
  ())

(defmethod generate-ghosts ((game game-state))
  (with-slots (ghosts level map) game
    (setf ghosts (list))
    (with-slots (ghost-spawn-gate ghost-spawns max-ghosts) map
      (let* ((position (copy-list ghost-spawn-gate))
             (new-ghost (make-instance 'ghost :index 0 :position position :game-state game)))
        (setf (ghost-strategy new-ghost) (make-instance 'track-follow :owner new-ghost))
        (push new-ghost ghosts))

      (loop with ghosts-with-abilities = (min level (- max-ghosts 1))
            for i from 1
            for spawn-place in ghost-spawns
            for position = (copy-list spawn-place)
            for new-ghost = (make-instance 'ghost :index i :position position :game-state game)
            do (setf (ghost-strategy new-ghost) (get-random-strategy new-ghost))
            when (> ghosts-with-abilities 0)
              do (setf (ghost-ability new-ghost) (get-random-ghost-ability))
                 (decf ghosts-with-abilities)
            do (push new-ghost ghosts)))))

(defmethod reset-game ((game game-state))
  (with-slots (map level score dots lives) game
    (setf level 1)
    (setf score 0)
    (setf dots (fill-with-dots map))
    (setf lives *default-lives*)))

(defmethod init-game ((game game-state))
  (with-slots (player ghosts stage map) game
    (with-slots (position ability) player ;; reset player state
      (setf position (copy-list (player-spawn map)))
      (setf ability nil))
    (generate-ghosts game)
    (setf stage 'start)))

(defmethod simulate-entities ((game game-state))
  (with-slots (dots ghosts level map player stage) game
    (loop for g in (game-ghosts game)
          do (move-and-check-collision g))
    (move-and-check-collision player)
    (when (= dots 0)
      (incf level)
      (setf dots (fill-with-dots map))
      (setf stage 'init)
      ;; (draw-dialog-with-timeout (format nil "Congratulations! You're not at level ~A!" level))
      ))
  ;; TODO: catch collision:
  ;; (decf lives)
  ;; (if (= lives 0)
  ;;     (setf stage 'defeat)
  ;;     (progn (setf stage 'init)
  ;;            (draw-dialog-with-timeout
  ;;              (format nil "You got caught, but you still have ~A ~A left!"
  ;;                      lives
  ;;                      (if (= lives 1) "life" "lives")))))
  )

(defmethod game-duration ((game game-state))
  (with-slots (time-at-pause timer-start) game
    (+ (- (get-universal-time) timer-start) time-at-pause)))

(defmethod draw-entities ((game game-state) renderer)
  (draw (game-player game) renderer)
  (loop for g in (game-ghosts game)
        do (draw g renderer)))

(defmethod draw-hud ((game game-state) renderer)
  (format t "Drawing HUD to the screen~%"))

(defun draw-dialog (renderer text)
  (format t "Drawing ~A to the screen~%" text))

(defmethod game-loop ((game game-state))
  (with-slots (map player stage time-at-pause timer-start) game
    (reset-game game)
    (recompute-draw-props +default-window-width+ +default-window-height+ map)

    (sdl2:with-init (:everything)
      (sdl2:with-window (window :title "puck-man"
                                :flags '(:input-focus :resizable :shown)
                                :w +default-window-width+ :h +default-window-height+)
        (sdl2:with-renderer (renderer window :flags '(:accelerated :targettexture))
          (sdl2:with-event-loop (:method :poll)
            (:windowevent (:event event :data1 width :data2 height)
                          (when (= event sdl2-ffi:+sdl-windowevent-resized+) ;; Window resized
                            (recompute-draw-props width height map)))
            (:keyup (:keysym keysym)
                    (let ((keycode (sdl2:scancode-value keysym)))
                      (if (sdl2:scancode= keycode :scancode-escape)
                          (sdl2:push-event :quit))
                      (case stage
                        (start (setf time-at-pause 0)
                               (setf timer-start (get-universal-time))
                               (setf stage 'countdown))
                        (countdown (when (sdl2:scancode= keycode :scancode-p)
                                     (setf stage 'paused))
                                   (set-next-dir player keycode))
                        (playing (when (sdl2:scancode= keycode :scancode-p)
                                   (setf stage 'paused)
                                   (incf time-at-pause (- (get-universal-time) timer-start)))
                                 (set-next-dir player keycode))
                        (paused (if (sdl2:scancode= keycode :scancode-p)
                                    (setf stage 'playing)))
                        (defeat (when (sdl2:scancode= keycode :scancode-r)
                                  (reset-game game)
                                  (setf stage 'init))))))
            (:idle ()
                   (sdl2:set-render-draw-color renderer 0 0 0 255)
                   (sdl2:render-clear renderer)
                   (draw (game-map game) renderer)

                   (ecase stage
                     (init      (init-game game))
                     (start     (draw-entities game renderer)
                                (draw-dialog renderer "Press any key to start the game."))
                     (countdown (draw-entities game renderer)
                                (draw-hud game renderer)
                                (when (> (- (get-universal-time) timer-start) *time-to-start*)
                                  (setf time-at-pause 0)
                                  (setf timer-start (get-universal-time))
                                  (setf stage 'playing)))
                     (playing   (simulate-entities game)
                                (draw-entities game renderer)
                                (draw-hud game renderer))
                     (paused    (draw-dialog renderer "Game paused. Press 'P' to unpause."))
                     (defeat    (draw-dialog renderer "Game over! Press 'R' to restart.")))

                   (sdl2:render-present renderer)
                   (sdl2:delay *frame-delay*))
            (:quit () t)))))))

(defparameter *default-map* (with-open-file (input "resources/default.map") (make-game-map input)))
(defun game-main ()
  (let* ((player (make-instance 'player :position (copy-list (player-spawn *default-map*))))
         (game-state (make-instance 'game-state :map *default-map* :player player)))
    (game-loop game-state)))

;; (push '*default-pathname-defaults* asdf:*central-registry*)
;; (asdf:load-system :puck-man)
;; (game-main)

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
  (:documentation "This class represents a basic game entity"))

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

(defmethod move-and-check-collision ((entity game-entity))
  (with-slots (direction position speed) entity
      (let* ((game (entity-game entity))
             (map (game-map game)))
        (loop for i below (entity-speed entity)
              for next-tile = (tile-at map (get-next-tile-pos position direction))
              when (can-traverse-tile-p entity next-tile)
                do (move-to-next-tile entity direction)
                   (when (portal-p next-tile)
                     (setf position (get-other-portal-pos map position)))
                   (when (check-collision entity)
                     nil) ;; TODO: signal collision
                   ))))

(defmethod move-to-next-tile ((entity game-entity) direction)
  (with-slots (position) entity
    (case direction
      (up    (decf (cdr position) (/ 1.0 64)))
      (left  (decf (car position) (/ 1.0 64)))
      (right (incf (cdr position) (/ 1.0 64)))
      (down  (incf (car position) (/ 1.0 64))))))

(defclass ghost (game-entity)
  ((direction :initform 'left
              :documentation "The direction that the ghost is facing")
   (alive :initform nil
          :accessor ghost-alive
          :documentation "T if the ghost is alive")
   (time-to-respawn :reader ghost-time-to-respawn
                    :documentation "Ghost respawn time")
   (tracking-strategy :initarg :tracking-strategy
                      :initform (error "no value for slot 'tracking-strategy'")
                      :reader ghost-strategy
                      :documentation "Ghost's tracking strategy")
   (special-ability :initform nil
                    :reader ghost-ability
                    :documentation "Ghost's special ability")))

(defmethod initialize-instance :after ((ghost ghost) &rest rest &key index &allow-other-keys)
  (declare (ignore rest))
  (with-slots (time-to-respawn tracking-strategy) ghost
    (setf time-to-respawn (* index *default-respawn-time*))
    (setf (slot-value tracking-strategy 'owner) ghost)))

(defmethod can-traverse-tile-p ((ghost ghost) tile)
  (and (member tile '(empty portal-A portal-B portal-C portal-D
                      dot super-dot spawn-gate player-spawn))
       t))

(defmethod check-collision ((ghost ghost))
  (with-slots (game position) ghost
    (equal position (entity-position (game-player game)))))

(defmethod move-and-check-collision :before ((ghost ghost))
  (with-slots (ability alive direction game position speed time-to-respawn tracking-strategy) ghost
    (when (and (not alive) (> (game-duration game) time-to-respawn))
      (setf alive t)
      (setf position (ghost-spawn-gate (game-map game)))
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
         (rect (sdl2:make-rect (+ (car *draw-start*) (* (car (entity-position ghost)) *tile-edge*))
                               (+ (cdr *draw-start*) (* (cdr (entity-position ghost)) *tile-edge*))
                               *tile-edge* *tile-edge*))
         (color (etypecase ghost-strategy
                  (track-follow '(192 0 255 255))
                  (track-patrol '(0 192 255 255))
                  (track-ambush '(192 192 0 255))
                  (track-random '(0 255 0 255)))))
    (apply #'sdl2:set-render-draw-color renderer color)
    (sdl2:render-fill-rect renderer rect)))

(defclass tracking-strategy ()
  ((owner :documentation "The ghost object that owns the strategy")
   (next-dir :documentation "Direction of the ghost's next move")))

(defgeneric get-target ())
(defgeneric get-next-dir ())
(defgeneric get-move (tracking-strategy))

(defclass track-follow (tracking-strategy)
  ())

(defclass track-patrol (tracking-strategy)
  ())

(defclass track-ambush (tracking-strategy)
  ())

(defclass track-random (tracking-strategy)
  ())

(defclass player (game-entity)
  ((position :documentation "Actual position (x . y) of the player")
   (direction :initform 'none
              :documentation "The direction that the player is facing")
   (next-direction :initform 'none
                   :reader player-next-dir
                   :documentation "Direction advice for the player")
   (apparent-position :reader player-position
                      :documentation "Player's apperent position (x . y)")
   (special-ability :initform nil
                    :documentation "The player's ability")))

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

(defmethod check-collision ((player player))
  (with-slots (ability game position) player
      (let* ((map (game-map game))
             (current-tile (tile-at map position)))
        (case current-tile
          (dot (progn (set-tile-at map position 'empty)
                      (incf (game-score game) 10)
                      (decf (game-dots game))))
          (super-dot (progn (set-tile-at map position 'empty)
                            (incf (game-score game) 100)
                            (setf ability (get-random-player-ability))))))))

(defmethod move-and-check-collision :before ((player player))
  (with-slots (ability apparent-position direction game next-direction position speed) player
    (setf apparent-position position)
    (let ((map (game-map game)))
      (when (and (next-tile-exists-p map position direction)
                 (can-traverse-tile-p player (tile-at map (get-next-tile-pos position direction))))
        (setf direction next-direction)
        (setf next-direction 'none)))
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
  (let ((rect (sdl2:make-rect (+ (car *draw-start*) (* (car (entity-position player)) *tile-edge*))
                              (+ (cdr *draw-start*) (* (cdr (entity-position player)) *tile-edge*))
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

(defun get-random-strategy ()
  (let ((class (ecase (random 4)
                 (0 'track-follow)
                 (1 'track-ambush)
                 (2 'track-patrol)
                 (3 'track-random))))
    (make-instance class)))

(defun get-random-ghost-ability ()
  ())

(defun get-random-player-ability ()
  ())

(defmethod generate-ghosts ((game game-state))
  (with-slots (ghosts level map) game
    (setf ghosts (list))
    (with-slots (ghost-spawn-gate ghost-spawns max-ghosts) map
      (push (make-instance 'ghost :index 0 :position ghost-spawn-gate
                                  :tracking-strategy (make-instance 'track-follow)
                                  :game-state game)
            ghosts)
      (loop with ghosts-with-abilities = (min level (- max-ghosts 1))
            for i from 1
            for spawn-place in ghost-spawns
            for new-ghost = (make-instance 'ghost :index i :position spawn-place
                                                  :tracking-strategy (get-random-strategy)
                                                  :game-state game)
            when (> ghosts-with-abilities 0)
              do (setf (slot-value new-ghost 'special-ability)
                       (get-random-ghost-ability))
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
    (with-slots (position actual-position special-ability) player ;; reset player state
      (setf position (player-spawn map))
      (setf actual-position (player-spawn map))
      (setf special-ability nil))
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
                        (countdown (if (sdl2:scancode= keycode :scancode-p)
                                       (setf stage 'paused)))
                        (playing (when (sdl2:scancode= keycode :scancode-p)
                                   (setf stage 'paused)
                                   (incf time-at-pause (- (get-universal-time) timer-start)))
                                 (set-next-dir player keycode)))
                        (paused (if (sdl2:scancode= keycode :scancode-p)
                                    (setf stage 'playing)))
                        (defeat (when (sdl2:scancode= keycode :scancode-r)
                                  (reset-game game)
                                  (setf stage 'init)))))
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
                   (sdl2:delay 33))
            (:quit () t)))))))

(defparameter *default-map* (with-open-file (input "resources/default.map") (make-game-map input)))
(defun game-main ()
  (let* ((player (make-instance 'player :position (player-spawn *default-map*)))
         (game-state (make-instance 'game-state :map *default-map* :player player)))
    (game-loop game-state)))

;; (push '*default-pathname-defaults* asdf:*central-registry*)
;; (asdf:load-system :puck-man)
;; (game-main)

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
(defgeneric get-move ())

(defclass track-follow (tracking-strategy)
  ())

(defclass track-patrol (tracking-strategy)
  ())

(defclass track-ambush (tracking-strategy)
  ())

(defclass track-random (tracking-strategy)
  ())

(defclass player (game-entity)
  ((position :documentation "Apparent position (x . y) of the player")
   (direction :initform 'none
              :documentation "The direction that the player is facing")
   (next-direction :initform 'none
                   :reader player-next-dir
                   :documentation "Direction advice for the player")
   (actual-position :reader player-actual-position
                      :documentation "Player's actual position (x . y)")
   (special-ability :initform nil
                    :documentation "The player's ability")))

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
   (level :initform 0)
   (score :initform 0)
   (dots :initform 0)
   (lives :initform 0)
   (timer-start :initform (get-time-of-day))
   (time-at-pause :initform 0)
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

(defmethod draw-entities ((game game-state) renderer)
  (draw (game-player game) renderer)
  (loop for g in (game-ghosts game)
        do (draw g renderer)))

(defmethod draw-dialog ((game game-state) text)
  (format t "Drawing ~A to the screen~%" text))

(defmethod draw-hud ((game game-state))
  (format t "Drawing HUD to the screen~%"))

(defmethod game-loop ((game game-state))
  (with-slots (map stage time-at-pause timer-start) game
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
                        (playing ())
                        (paused (if (sdl2:scancode= keycode :scancode-p)
                                    (setf stage 'playing)))
                        (defeat ()))))
            (:idle ()
                   (sdl2:set-render-draw-color renderer 0 0 0 255)
                   (sdl2:render-clear renderer)
                   (draw (game-map game) renderer)

                   (ecase stage
                     (init      (init-game game))
                     (start     (draw-entities game renderer)
                                (draw-dialog "Press any key to start the game."))
                     (countdown (draw-entities game renderer)
                                (draw-hud)
                                (when (> (- (get-universal-time) timer-start) *time-to-start*)
                                  (setf time-at-pause 0)
                                  (setf timer-start (get-universal-time))
                                  (setf stage 'playing)))
                     (playing )
                     (paused    (draw-dialog "Game paused. Press 'P' to unpause."))
                     (defeat))

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

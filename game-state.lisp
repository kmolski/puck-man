(require :alexandria)
(require :sdl2)
(require :str)
(require :trivia)

(setf *random-state* (make-random-state t))

;; window stuff

(defconstant +default-window-width+ 800)
(defconstant +default-window-height+ 600)

(defun get-tile-edge (window-width window-height map)
  (let* ((map-dimensions (array-dimensions (map-tiles map)))
         (map-width  (first map-dimensions))
         (map-height (second map-dimensions)))
    (round (min (/ window-width map-width)
                (/ window-height map-height)))))

(defun get-draw-start (window-width window-height map)
  (let* ((map-dimensions (array-dimensions (map-tiles map)))
         (map-width  (first map-dimensions))
         (map-height (second map-dimensions)))
    (cons (floor (- (/ window-width  2) (/ (* map-width *tile-edge*)  2)))
          (floor (- (/ window-height 2) (/ (* map-height *tile-edge*) 2))))))

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
  (setf (slot-value ghost 'time-to-respawn) (* index *default-respawn-time*)))

(defmethod draw ((ghost ghost) renderer)
  "Draw the ghost with the given renderer."
  (let* ((ghost-strategy (ghost-strategy ghost))
         (rect (sdl2:make-rect (+ (car *draw-start*) (* (car (entity-position ghost)) *tile-edge*))
                               (+ (cdr *draw-start*) (* (cdr (entity-position ghost)) *tile-edge*))
                               *tile-edge* *tile-edge*))
         (color (cond
                  ((eq ghost-strategy #'track-follow) '(192 0 255 255))
                  ((eq ghost-strategy #'track-patrol) '(0 192 255 255))
                  ((eq ghost-strategy #'track-ambush) '(192 192 0 255))
                  ((eq ghost-strategy #'track-random) '(0 255 0 255))
                  (t '(255 255 255 255)))))
    (apply #'sdl2:set-render-draw-color renderer color)
    (sdl2:render-fill-rect renderer rect)))

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
  (ecase (random 4)
    (0 #'track-follow)
    (1 #'track-ambush)
    (2 #'track-patrol)
    (3 #'track-random)))

(defun track-follow () ())
(defun track-ambush () ())
(defun track-patrol () ())
(defun track-random () ())

(defun get-random-ghost-ability ()
  ())

(defmethod generate-ghosts ((game game-state))
  (with-slots (ghosts level map) game
    (setf ghosts (list))
    (with-slots (ghost-spawn-gate ghost-spawns max-ghosts) map
      (push (make-instance 'ghost :index 0 :position ghost-spawn-gate
                                  :game-state game :tracking-strategy #'track-follow)
            ghosts)
      (loop with ghosts-with-abilities = (min level (- max-ghosts 1))
            for i from 1
            for spawn-place in ghost-spawns
            for new-ghost = (make-instance 'ghost :index i :position spawn-place
                                                  :game-state game :tracking-strategy (get-random-strategy))
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
    ;; Reset player state
    (with-slots (position actual-position special-ability) player
      (setf position (player-spawn map))
      (setf actual-position (player-spawn map))
      (setf special-ability nil))
    (generate-ghosts game)
    (setf stage 'start)))
(defmethod game-loop ((game game-state))
  (with-slots (stage) game
    (reset-game game)
    (sdl2:with-init (:everything)
      (sdl2:with-window (window :title "puck-man"
                                :flags '(:input-focus :resizable :shown)
                                :w +default-window-width+ :h +default-window-height+)
        (sdl2:with-renderer (renderer window :flags '(:accelerated :targettexture))
          (sdl2:with-event-loop (:method :poll)
            (:windowevent (:event event :data1 width :data2 height)
                          (when (= event sdl2-ffi:+sdl-windowevent-resized+) ;; Window resized
                            (setf *tile-edge* (get-tile-edge width height (game-map game)))
                            (setf *draw-start* (get-draw-start width height (game-map game)))))
            (:keyup (:keysym keysym)
                    (let ((keycode (sdl2:scancode-value keysym)))
                      (when (sdl2:scancode= keycode :scancode-escape)
                        (sdl2:push-event :quit))
                      (case stage
                        (start ())
                        (countdown ())
                        (playing ())
                        (paused ())
                        (defeat ()))))
            (:idle ()
                   (sdl2:set-render-draw-color renderer 0 0 0 255)
                   (sdl2:render-clear renderer)
                   (draw (game-map game) renderer)

                   (ecase stage
                     (init (init-game game))
                     (start (start-game game renderer))
                     (countdown )
                     (playing )
                     (paused )
                     (defeat))

                   (sdl2:render-present renderer)
                   (sdl2:delay 33))
            (:quit () t)))))))

(defparameter *default-map* (with-open-file (input "resources/default.map") (make-game-map input)))
(defparameter *tile-edge* (get-tile-edge +default-window-width+ +default-window-height+ *default-map*))
(defparameter *draw-start* (get-draw-start +default-window-width+ +default-window-height+ *default-map*))

(defun game-main ()
  (let* ((player (make-instance 'player :position (player-spawn *default-map*)))
         (game-state (make-instance 'game-state
                                    :map *default-map*
                                    :player player)))
    (game-loop game-state)))

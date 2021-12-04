(require :sdl2)
(require :sdl2-image)

(setf *random-state* (make-random-state t))

(defparameter *time-to-start* 10)
(defparameter *default-lives* 3)

(defclass game-state ()
  ((map :initarg :map
        :reader game-map
        :initform (error "no value for slot 'map'")
        :documentation "The game map object")
   (spritemap :initarg :spritemap
              :initform (error "no value for slot 'spritemap'")
              :documentation "SDL2 surface that contains the spritemap")
   (player :initarg :player
           :reader game-player
           :initform (error "no value for slot 'player'"))
   (ghosts :reader game-ghosts)
   (level :initform 1)
   (score :accessor game-score
          :initform 0)
   (dots :accessor game-dots
         :initform 0)
   (lives :initform *default-lives* :documentation "Remaining lives of the player")
   (timer-start :documentation "Timestamp of the game start")
   (time-at-pause :documentation "Game time spent paused (in seconds)")
   (stage :initform 'init))
  (:documentation "Representation of the game state"))

(defmethod initialize-instance :after ((game game-state) &rest rest)
  "Initialize the player's game-state field."
  (declare (ignore rest))
  (with-slots (game-state) (game-player game)
    (setf game-state game)))

(defun get-random-strategy (owner &optional (max-index 4))
  "Return a randomly chosen tracking strategy for the ghost."
  (let ((class (ecase (random max-index)
                 (0 'track-follow)
                 (1 'track-ambush)
                 (2 'track-patrol)
                 (3 'track-random))))
    (make-instance class :owner owner)))

(defmethod generate-ghosts ((game game-state))
  "Generate different ghosts based on the current level and map properties."
  (with-slots (ghosts level map) game
    (setf ghosts (list))
    (with-slots (ghost-spawn-gate ghost-spawns max-ghosts) map
      (let* ((position (copy-list ghost-spawn-gate))
             (new-ghost (make-instance 'ghost :index 0 :position position :game-state game)))
        (setf (ghost-strategy new-ghost) (make-instance 'track-follow :owner new-ghost))
        (push new-ghost ghosts))
      (loop for i from 1
            for spawn-place in ghost-spawns
            for position = (copy-list spawn-place)
            for new-ghost = (make-instance 'ghost :index i :position position :game-state game)
            do (setf (ghost-strategy new-ghost) (get-random-strategy new-ghost))
               (push new-ghost ghosts)))))

(defmethod reset-game ((game game-state))
  "Reset the game state (level, score, dots and lives)."
  (with-slots (map level score dots lives) game
    (setf level 1)
    (setf score 0)
    (setf dots (fill-with-dots map))
    (setf lives *default-lives*)))

(defmethod init-game ((game game-state))
  "Start the game: generate ghosts, set player position and next stage."
  (with-slots (player ghosts stage map) game
    (with-slots (position) player ;; reset player state
      (setf position (copy-list (player-spawn map))))
    (generate-ghosts game)
    (setf stage 'start)))

(defmethod handle-collision ((game game-state))
  "Handle a collision between a player and a ghost."
  (with-slots (lives stage) game
    (decf lives)
    (if (= lives 0)
        (setf stage 'defeat)
        (progn (setf stage 'init)
               ;; (draw-dialog-with-timeout
               ;;  (format nil "You got caught, but you still have ~A ~A left!" lives
               ;;          (if (= lives 1) "life" "lives")))
               ))))

(defmethod simulate-entities ((game game-state))
  "Simulate a single game tick - move entities forward, check collisions and check the win condition."
  (with-slots (dots ghosts level map player stage) game
    (loop for g in (game-ghosts game)
          do (move-and-check-collision g))
    (move-and-check-collision player)
    (when (= dots 0)
      (incf level)
      (setf dots (fill-with-dots map))
      (setf stage 'init)
      ;; (draw-dialog-with-timeout (format nil "Congratulations! You're not at level ~A!" level))
      )))

(defmethod game-duration ((game game-state))
  "Compute the total game duration."
  (with-slots (time-at-pause timer-start) game
    (+ (- (get-universal-time) timer-start) time-at-pause)))

(defmethod draw-entities ((game game-state) renderer)
  "Draw the entities with the given renderer."
  (draw (game-player game) renderer)
  (loop for g in (game-ghosts game)
        do (draw g renderer)))

(defmethod draw-hud ((game game-state) renderer)
  "Draw the HUD with the given renderer."
  ;; (format t "Drawing HUD to the screen~%")
  )

(defun draw-dialog (renderer text)
  "Draw a dialog with the given renderer."
  ;; (format t "Drawing ~A to the screen~%" text)
  )

(defun draw-dialog-with-timeout (window renderer text)
  "Draw a dialog with the given renderer, blocking the app for some time."
  ())

(defmethod game-loop ((game game-state))
  "The main game loop."
  (with-slots (map player spritemap stage time-at-pause timer-start) game
    (reset-game game)
    (recompute-draw-props +default-window-width+ +default-window-height+ map)

    (sdl2:with-init (:everything)
      (sdl2:with-window (window :title "puck-man"
                                :flags '(:input-focus :resizable :shown)
                                :w +default-window-width+ :h +default-window-height+)
        (sdl2:with-renderer (renderer window :flags '(:accelerated :targettexture))
          (setf *spritemap-texture* (sdl2:create-texture-from-surface renderer spritemap))
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
                     (playing   (handler-case (simulate-entities game)
                                  (entity-collision () (handle-collision game)))
                                (draw-entities game renderer)
                                (draw-hud game renderer))
                     (paused    (draw-dialog renderer "Game paused. Press 'P' to unpause."))
                     (defeat    (draw-dialog renderer "Game over! Press 'R' to restart.")))

                   (sdl2:render-present renderer)
                   (sdl2:delay *frame-delay*))
            (:quit () t)))))))

(defparameter *default-map* (with-open-file (input "resources/default.map") (make-game-map input)))
(defparameter *default-spritemap* (sdl2-image:load-png-rw "resources/spritemap.png"))
(defun game-main ()
  "Entry point for the game."
  (let* ((player (make-instance 'player :position (copy-list (player-spawn *default-map*))))
         (game-state (make-instance 'game-state :map *default-map* :spritemap *default-spritemap* :player player)))
    (game-loop game-state)))

;; (push '*default-pathname-defaults* asdf:*central-registry*)
;; (asdf:load-system :puck-man)
;; (game-main)

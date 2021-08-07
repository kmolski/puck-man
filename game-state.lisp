(require :alexandria)
(require :sdl2)
(require :str)
(require :trivia)

(setf *random-state* (make-random-state t))

;; map stuff

(defparameter *max-superdots* 5)

(defun get-map-dimensions (input-stream)
  (loop for line = (read-line input-stream nil)
        while line
            maximize (length line) into width
            count line into height
        finally (file-position input-stream 0)
                (return (list height width))))

(defun char->tile (character)
  (trivia:match character
    ((or #\- #\|) 'wall)
    (#\A          'portalA)
    (#\B          'portalB)
    (#\C          'portalC)
    (#\D          'portalD)
    (#\.          'inaccessible)
    (#\#          'spawn-gate)
    (#\G          'ghost-spawn)
    (#\P          'player-spawn)
    (_            'empty)))

(defun portal-p (tile)
  (trivia:match tile
    ((or 'portalA 'portalB 'portalC 'portalD) t)
    (_ nil)))

(defun make-game-map (input-stream)
  (let* ((map-dimensions (get-map-dimensions input-stream))
         (tile-array (make-array map-dimensions :initial-element 'inaccessible))
         (current-pos (list 0 0))
         (ghost-spawn-gate)
         (ghost-spawns (list))
         (player-spawn)
         (portals (list)))
    (when (= (apply #'* map-dimensions) 0)
      (error "The provided map is empty!"))
    (loop for char = (read-char input-stream nil)
          for pos  = (copy-list current-pos)
          while char
          if (char/= char #\Newline)
             do (let ((tile (char->tile char)))
                  (trivia:match tile
                    ('spawn-gate   (setf ghost-spawn-gate pos))
                    ('ghost-spawn  (push pos ghost-spawns))
                    ('player-spawn (setf player-spawn pos))
                    ((or 'portalA 'portalB 'portalC 'portalD)
                         (if (getf portals tile)
                             (rplacd (getf portals tile) pos)
                             (setf (getf portals tile) (cons pos nil)))))
                  (setf (aref tile-array (first pos) (second pos)) tile)
                  (incf (second current-pos)))
          else
             do (progn (setf (second current-pos) 0)
                       (incf (first current-pos))))
    (make-instance 'game-map
                   :tiles tile-array
                   :player-spawn player-spawn
                   :ghost-spawn-gate ghost-spawn-gate
                   :ghost-spawns ghost-spawns
                   :max-ghosts (length ghost-spawns)
                   :portals portals)))

(defclass game-map ()
  ((tiles :initarg :tiles
          :initform (error "no value for slot 'tiles'")
          :reader map-tile-at)
   (player-spawn :initarg :player-spawn)
   (ghost-spawn-gate :initarg :ghost-spawn-gate)
   (ghost-spawns :initarg :ghost-spawns)
   (max-ghosts :initarg :max-ghosts)
   (portals :initarg :portals)))

;; game model stuff

(defparameter *time-to-start* 10)
(defparameter *default-lives* 3)

(defclass game-state ()
  ((map :initarg :map
        :initform (error "no value for slot 'map'"))
   (player :initarg :player
           :initform (error "no value for slot 'player'"))
   (ghosts :initform (list))
   (window :initarg window
           :initform (error "no value for slot 'window'"))
   (level :initform 0)
   (score :initform 0)
   (dots :initform 0)
   (lives :initform 0)
   (timer-start :initform (get-time-of-day))
   (time-at-pause :initform 0)
   (stage :initform 'init)))

(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defun sdl2-test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "SDL2 test"
                           :flags '(:shown)
                           :w *window-width*
                           :h *window-height*)
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup (:keysym keysym)
                  (when (sdl2:scancode= (sdl2:scancode-value keysym)
                                        :scancode-escape)
                    (sdl2:push-event :quit)))
          (:idle ()
                 (multiple-value-bind (rects num)
                     (apply #'sdl2:rects*
                            (loop for x from 50 to (- *window-width* 50) by 150
                                  append (loop for y from 50 to (- *window-height* 50) by 150
                                               collect (sdl2:make-rect x y 100 100))))
                   (sdl2:set-render-draw-color renderer 255 127 0 255)
                   (sdl2:render-fill-rects renderer rects num))
                 (sdl2:render-present renderer)
                 (sdl2:delay 33))
          (:quit () t))))))

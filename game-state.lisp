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
         (ghost-spawns (list))
         (portals (list))
         (ghost-spawn-gate)
         (player-spawn))
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
    (make-instance 'game-map :tiles tile-array
                             :ghost-spawns ghost-spawns
                             :portals portals
                             :ghost-spawn-gate ghost-spawn-gate
                             :player-spawn player-spawn)))

(defclass game-map ()
  ((tiles :initarg :tiles
          :initform (error "no value for slot 'tiles'")
          :reader map-tiles)
   (ghost-spawns :initarg :ghost-spawns
                 :initform (error "no value for slot 'ghost-spawns'")
                 :reader ghost-spawns)
   (portals :initarg :portals
            :initform (error "no value for slot 'portals'")
            :reader map-portals)
   (ghost-spawn-gate :initarg :ghost-spawn-gate
                     :initform (error "no value for slot 'ghost-spawn-gate'")
                     :reader ghost-spawn-gate)
   (player-spawn :initarg :player-spawn
                 :initform (error "no value for slot 'player-spawn'")
                 :reader player-spawn)
   (max-ghosts :reader max-ghosts)))

(defmethod initialize-instance :after ((map game-map) &rest rest)
  (declare (ignore rest))
  (setf (slot-value map 'max-ghosts) (length (ghost-spawns map))))

(defun get-next-tile (position direction)
  (destructuring-bind (y x) position
    (trivia:match direction
      ('up    (list (1- y) x))
      ('left  (list y (1- x)))
      ('down  (list (1+ y) x))
      ('right (list y (1+ x)))
      (_      (list y x)))))

(defmethod tile-at ((map game-map) position)
  (destructuring-bind (y x) position
    (aref (map-tiles map) y x)))

(defmethod get-other-portal ((map game-map) position)
  ())

(defmethod next-tile-exists-p ((map game-map) position direction)
  ())

(defmethod fill-with-dots ((map game-map))
  ())

(defmethod remove-all-dots ((map game-map))
  ())

(defmethod draw ((map game-map))
  ())

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

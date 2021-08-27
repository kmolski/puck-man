;; (defpackage :pl.kmolski.puck-man (:use :asdf :cl))
;; (in-package :pl.kmolski.puck-man)

(require :alexandria)
(require :sdl2)
(require :str)
(require :trivia)

(setf *random-state* (make-random-state t))

;; map stuff

(defparameter *max-superdots* 5)

(defun get-map-dimensions (input-stream)
  "Get the map dimensions from the provided input stream. The height of the
   map is the line count, while the width is the maximum line length."
  (loop for line = (read-line input-stream nil)
        while line
            maximize (length line) into width
            count line into height
        ;; Reset the file position so that the stream can be used again
        finally (file-position input-stream 0)
                (return (list width height))))

(defun char->tile (character)
  "Convert a character to a map tile."
  (trivia:match character
    ((or #\- #\|) 'wall)
    (#\A          'portal-A)
    (#\B          'portal-B)
    (#\C          'portal-C)
    (#\D          'portal-D)
    (#\.          'inaccessible)
    (#\#          'spawn-gate)
    (#\G          'ghost-spawn)
    (#\P          'player-spawn)
    (_            'empty)))

(defun portal-p (tile)
  "Returns T if the provided tile is a portal."
  (and (member tile '(portal-A portal-B portal-C portal-D)) t))

(defun make-game-map (input-stream)
  "Create a game-map object from an input stream of map tiles/characters."
  (let* ((map-dimensions (get-map-dimensions input-stream))
         (tile-array (make-array map-dimensions :initial-element 'inaccessible))
         (current-pos (list 0 0))
         (ghost-spawns (list))
         (portals (list))
         (ghost-spawn-gate)
         (player-spawn))
    (when (= 0 (apply #'* map-dimensions))
      (error "The provided map is empty!"))
    (loop for char = (read-char input-stream nil)
          for pos  = (copy-list current-pos)
          while char
          if (char/= char #\Newline) ; Proper map tile
             do (let ((tile (char->tile char)))
                  (trivia:match tile
                    ('spawn-gate   (setf ghost-spawn-gate pos))
                    ('ghost-spawn  (push pos ghost-spawns))
                    ('player-spawn (setf player-spawn pos))
                    ((or 'portal-A 'portal-B 'portal-C 'portal-D)
                         (if (getf portals tile) ; If a record for the portal exists
                             ;; Set the other end of the mapping
                             (setf (cdr (getf portals tile)) pos)
                             ;; If it doesn't, create the mapping
                             (setf (getf portals tile) (cons pos nil)))))
                  (setf (aref tile-array (first pos) (second pos)) tile)
                  (incf (first current-pos)))
          else ; Next tile row, start from x=0
             do (setf (first current-pos) 0)
                (incf (second current-pos)))
    (when (not player-spawn)
      (error "There must be a player spawn ('P') on the map!"))
    (when (not ghost-spawn-gate)
      (error "There must be a ghost spawn gate ('#') on the map!"))
    (when (= 0 (length ghost-spawns))
      (error "There must be at least one ghost spawn ('G') on the map!"))
    (loop for (portal-name portal-mapping) on portals by #'cddr
          when (not (cdr portal-mapping))
            do (error "Incorrect mapping for ~a!" portal-name))
    (make-instance 'game-map :tiles tile-array
                             :ghost-spawns ghost-spawns
                             :portals portals
                             :ghost-spawn-gate ghost-spawn-gate
                             :player-spawn player-spawn)))

(defclass game-map ()
  ((tiles :initarg :tiles
          :initform (error "no value for slot 'tiles'")
          :reader map-tiles
          :documentation "2D array of map tiles")
   (ghost-spawns :initarg :ghost-spawns
                 :initform (error "no value for slot 'ghost-spawns'")
                 :reader ghost-spawns
                 :documentation "List of ghost spawn positions (x y)")
   (portals :initarg :portals
            :initform (error "no value for slot 'portals'")
            :reader map-portals
            :documentation "Plist of (portal-name ((x1 y1) . (x2 y2)) ...)")
   (ghost-spawn-gate :initarg :ghost-spawn-gate
                     :initform (error "no value for slot 'ghost-spawn-gate'")
                     :reader ghost-spawn-gate
                     :documentation "Ghost spawn gate position (x y)")
   (player-spawn :initarg :player-spawn
                 :initform (error "no value for slot 'player-spawn'")
                 :reader player-spawn
                 :documentation "Player spawn position (x y)")
   (max-ghosts :reader max-ghosts
               :documentation "Max amount of ghosts, based on the ghost spawn count")))

(defmethod initialize-instance :after ((map game-map) &rest rest)
  (declare (ignore rest))
  (setf (slot-value map 'max-ghosts) (length (ghost-spawns map))))

(defun get-next-tile-pos (position direction)
  "Get the position (x y) of the next tile in the given direction,
   starting from the given position (x y)."
  (destructuring-bind (x y) position
    (trivia:match direction
      ('up    (list x (1- y)))
      ('left  (list (1- x) y))
      ('down  (list x (1+ y)))
      ('right (list (1+ x) y))
      (_      (error "invalid direction")))))

(defmethod tile-at ((map game-map) position)
  "Get the tile at position (x y)."
  (destructuring-bind (x y) position
    (aref (map-tiles map) x y)))

(defmethod get-other-portal-pos ((map game-map) position)
  "Get the position of the other portal from the pair."
  (let* ((tile (tile-at map position))
         (portal-pair (getf (map-portals map) tile)))
    (if (equal (car portal-pair) position)
        (cdr portal-pair)
        (car portal-pair))))

(defmethod next-tile-exists-p ((map game-map) position direction)
  "Return T if the next tile in the given direction exists."
  (let ((tile-array-dims (array-dimensions (map-tiles map)))
        (pos-x (first position))
        (pos-y (second position)))
    (trivia:match direction
      ('up    (> (floor pos-y) 0))
      ('left  (> (floor pos-x) 0))
      ('down  (< (ceiling pos-y) (1- (second tile-array-dims))))
      ('right (< (ceiling pos-x) (1- (first tile-array-dims))))
      (_      (error "invalid direction")))))

(defmethod fill-with-dots ((map game-map))
  "Fill the map with regular and super dots."
  (remove-all-dots map) ; Start by removing the remaining dots
  (let* ((map-tiles (map-tiles map))
         (map-dimensions (array-dimensions map-tiles))
         (map-width  (first map-dimensions))
         (map-height (second map-dimensions))
         (superdots-left *max-superdots*)
         (dot-count 0))
    (loop while (> superdots-left 0) ; Place super-dots in random positions
          for rand-x = (random map-width)
          for rand-y = (random map-height)
          when (eql (aref map-tiles rand-x rand-y) 'empty)
            do (setf (aref map-tiles rand-x rand-y) 'super-dot)
               (decf superdots-left))
    (loop for y below map-height
          do (loop for x below map-width
                   when (eql (aref map-tiles x y) 'empty)
                     do (setf (aref map-tiles x y) 'dot)
                        (incf dot-count)))
    dot-count))

(defmethod remove-all-dots ((map game-map))
  "Remove all dots and super-dots from the map."
  (let* ((map-tiles (map-tiles map))
         (map-dimensions (array-dimensions map-tiles))
         (map-width  (first map-dimensions))
         (map-height (second map-dimensions)))
    (loop for y below map-height
          do (loop for x below map-width
                   for tile = (aref map-tiles x y)
                   when (or (eql tile 'dot) (eql tile 'super-dot))
                     do (setf (aref map-tiles x y) 'empty)))))

(defmethod draw ((map game-map) renderer)
  "Draw the map with the given renderer."
  ;; TODO: render the map to a texture here!
  ())

;; entity stuff

(defparameter *default-respawn-time* 30)

(defclass game-entity ()
  ((position :initarg :position
             :initform (error "no value for slot 'position'")
             :reader entity-position
             :documentation "The position (x y) of the entity")
   (direction :initarg :direction
              :initform (error "no value for slot 'direction'")
              :reader entity-direction
              :documentation "The direction that the entity is facing")
   (speed :initarg :default-speed
          :initform (error "no value for slot 'speed'")
          :reader entity-speed
          :documentation "The speed of the entity")
   (game-state :initarg :game-state
               :initform (error "no value for slot 'game-state'")
               :reader entity-game
               :documentation "The game that the entity belongs to"))
  (:documentation "This class represents a basic game entity"))

(defgeneric can-traverse-tile-p (entity tile)
  (:documentation "Checks if the entity can enter the map tile"))

(defgeneric check-collision (entity)
  (:documentation "Checks if there is a collision between the player and ghost"))

(defgeneric move (entity)
  (:documentation "Move the entity to a new position based on its speed and direction"))

(defmethod draw ((entity game-entity) renderer)
  "Draw the entity with the given renderer."
  ())

(defclass ghost (game-entity)
  ((alive :initform nil
          :accessor ghost-alive
          :documentation "T if the ghost is alive")
   (time-to-respawn :reader ghost-time-to-respawn
                    :documentation "Ghost respawn time")
   (tracking-strategy :initarg tracking-strategy
                      :initform (error "no value for slot 'tracking-strategy'")
                      :reader ghost-strategy
                      :documentation "Ghost's tracking strategy")
   (special-ability :initarg special-ability
                    :initform (error "no value for slot 'special-ability'")
                    :reader ghost-ability
                    :documentation "Ghost's special ability")))

(defmethod initialize-instance :after ((ghost ghost) &rest rest &key index &allow-other-keys)
  (declare (ignore rest))
  (setf (slot-value ghost 'time-to-respawn) (* index *default-respawn-time*)))

(defclass player (game-entity)
  ((next-direction :initform 'none
                   :reader player-next-dir
                   :documentation "Direction advice for the player")
   (reported-position :reader player-reported-position
                      :documentation "Player's position (x y) as seen by other entities")
   (special-ability :initform nil
                    :documentation "The player's ability")))

;; game model stuff

(defparameter *time-to-start* 10)
(defparameter *default-lives* 3)

(defclass game-state ()
  ((map :initarg :map
        :reader game-map
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
(defparameter *default-map* (with-open-file (input "resources/default.map") (make-game-map input)))

(defmethod game-loop ((game game-state))
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "puck-man"
                              :flags '(:input-grabbed :resizable :shown)
                              :w *window-width* :h *window-height*)
      (sdl2:with-renderer (renderer window :flags '(:accelerated :targettexture))
        (sdl2:with-event-loop (:method :poll)
          (:keyup (:keysym keysym)
                  (when (sdl2:scancode= (sdl2:scancode-value keysym)
                                        :scancode-escape)
                    (sdl2:push-event :quit)))
          (:idle ()
                 (draw (game-map game) renderer))
          (:quit () t))))))

(defun game-main ()
  (let ((game-state (make-instance 'game-state :map *default-map*)))
    (game-loop game-state)))

(defun sdl2-test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "SDL2 test"
                           :flags '(:shown :resizable)
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

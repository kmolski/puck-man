(require :sdl2)

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
  (case character
    ((#\- #\|) 'wall)
    (#\A       'portal-A)
    (#\B       'portal-B)
    (#\C       'portal-C)
    (#\D       'portal-D)
    (#\.       'inaccessible)
    (#\#       'spawn-gate)
    (#\G       'ghost-spawn)
    (#\P       'player-spawn)
    (otherwise 'empty)))

(defun portal-p (tile)
  "Returns T if the provided tile is a portal."
  (and (member tile '(portal-A portal-B portal-C portal-D)) t))

(defun make-game-map (input-stream)
  "Create a game-map object from an input stream of map tiles/characters."
  (let* ((map-dimensions (get-map-dimensions input-stream))
         (tile-array (make-array map-dimensions :initial-element 'inaccessible))
         (current-pos (cons 0 0))
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
                  (case tile
                    (spawn-gate   (setf ghost-spawn-gate pos))
                    (ghost-spawn  (push pos ghost-spawns))
                    (player-spawn (setf player-spawn pos))
                    ((portal-A portal-B portal-C portal-D)
                         (if (getf portals tile) ; If a record for the portal exists
                             ;; Set the other end of the mapping
                             (setf (cdr (getf portals tile)) pos)
                             ;; If it doesn't, create the mapping
                             (setf (getf portals tile) (cons pos nil)))))
                  (setf (aref tile-array (car pos) (cdr pos)) tile)
                  (incf (car current-pos)))
          else ; Next tile row, start from x=0
             do (setf (car current-pos) 0)
                (incf (cdr current-pos)))
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
                 :documentation "List of ghost spawn positions (x . y)")
   (portals :initarg :portals
            :initform (error "no value for slot 'portals'")
            :reader map-portals
            :documentation "Plist of (portal-name ((x1 . y1) . (x2 . y2)) ...)")
   (ghost-spawn-gate :initarg :ghost-spawn-gate
                     :initform (error "no value for slot 'ghost-spawn-gate'")
                     :reader ghost-spawn-gate
                     :documentation "Ghost spawn gate position (x . y)")
   (player-spawn :initarg :player-spawn
                 :initform (error "no value for slot 'player-spawn'")
                 :reader player-spawn
                 :documentation "Player spawn position (x . y)")
   (max-ghosts :reader max-ghosts
               :documentation "Max amount of ghosts, based on the ghost spawn count")
   (texture :reader map-texture
            :documentation "Cached SDL2 texture of the whole map")))

(defmethod initialize-instance :after ((map game-map) &rest rest)
  (declare (ignore rest))
  (setf (slot-value map 'max-ghosts) (length (ghost-spawns map))))

(defun get-next-tile-pos (position direction)
  "Get the position (x . y) of the next tile in the given direction,
   starting from the given position (x . y)."
  (destructuring-bind (x . y) position
    (ecase direction
      (up    (cons x (1- y)))
      (left  (cons (1- x) y))
      (down  (cons x (1+ y)))
      (right (cons (1+ x) y)))))

(defmethod tile-at ((map game-map) position)
  "Get the tile at position (x . y)."
  (destructuring-bind (x . y) position
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
        (pos-x (car position))
        (pos-y (cdr position)))
    (ecase direction
      (up    (> (floor pos-y) 0))
      (left  (> (floor pos-x) 0))
      (down  (< (ceiling pos-y) (1- (second tile-array-dims))))
      (right (< (ceiling pos-x) (1- (first tile-array-dims)))))))

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
  ;; TODO: render the map to a cached texture here!
  ;; TODO: extract get-map-dimensions
  (let* ((map-dimensions (array-dimensions (map-tiles map)))
         (map-width  (first map-dimensions))
         (map-height (second map-dimensions))
         (tile-edge (round (min (/ *window-width* map-width) (/ *window-height* map-height))))
         (draw-start (cons (floor (- (/ *window-width*  2) (/ (* map-width tile-edge)  2)))
                           (floor (- (/ *window-height* 2) (/ (* map-height tile-edge) 2))))))
    (loop for y-index below map-height
          for y = (+ (cdr draw-start) (* y-index tile-edge))
          append (loop for x-index below map-width
                       for x = (+ (car draw-start) (* x-index tile-edge))
                       for tile = (tile-at map (cons x-index y-index))
                       for color = (case tile
                                     (wall      '(0 0 255 255))
                                     (otherwise '(0 0 0 0)))
                       for rect = (sdl2:make-rect x y tile-edge tile-edge)
                       do (apply #'sdl2:set-render-draw-color renderer color)
                          (sdl2:render-fill-rect renderer rect)))))

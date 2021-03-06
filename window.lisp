(require :sdl2)

(defconstant +default-window-width+ 800)
(defconstant +default-window-height+ 600)
(defvar *tile-edge*)
(defvar *draw-start*)

(defconstant +spritemap-tile-size+ 12)
(defconstant +spritemap-entity-size+ (* +spritemap-tile-size+ 2))
(defvar *spritemap-texture*)

(defparameter *frame-delay* 32)

(defun get-tile-edge (window-width window-height map-width map-height)
  "Calculate the map tile edge length (in pixels) based on the window width+height and map size."
  (round (min (/ window-width map-width)
              (/ window-height map-height))))

(defun get-draw-start (window-width window-height map-width map-height)
  "Calculate the map position on the screen, based on the window width+height and map size."
  (cons (floor (- (/ window-width  2) (/ (* map-width *tile-edge*)  2)))
        (floor (- (/ window-height 2) (/ (* map-height *tile-edge*) 2)))))

(defun recompute-draw-props (width height map)
  "Recompute and update the map tile edge and position."
  (let* ((map-dimensions (array-dimensions (map-tiles map)))
         (map-width  (first map-dimensions))
         (map-height (second map-dimensions)))
    (setf *tile-edge* (get-tile-edge width height map-width map-height))
    (setf *draw-start* (get-draw-start width height map-width map-height))))

(defun make-sprite-vector (tile-size x-offset y-offset row-length)
  "Create a vector of sprite resource descriptors (SDL2 rects with the sprite inside)."
  (map 'vector #'identity
       (loop for i below row-length
             collect (sdl2:make-rect (+ (* i tile-size) x-offset) y-offset
                                     tile-size tile-size))))

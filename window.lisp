(defconstant +default-window-width+ 800)
(defconstant +default-window-height+ 600)
(defvar *tile-edge*)
(defvar *draw-start*)

(defun get-tile-edge (window-width window-height map)
  "Calculate the map tile edge length (in pixels) based on
  the window width+height and map dimenstions."
  (let* ((map-dimensions (array-dimensions (map-tiles map)))
         (map-width  (first map-dimensions))
         (map-height (second map-dimensions)))
    (round (min (/ window-width map-width)
                (/ window-height map-height)))))

(defun get-draw-start (window-width window-height map)
  "Calculate the map position on the screen, based on
  the window width+height and map dimenstions."
  (let* ((map-dimensions (array-dimensions (map-tiles map)))
         (map-width  (first map-dimensions))
         (map-height (second map-dimensions)))
    (cons (floor (- (/ window-width  2) (/ (* map-width *tile-edge*)  2)))
          (floor (- (/ window-height 2) (/ (* map-height *tile-edge*) 2))))))

(defun recompute-draw-props (width height map)
  "Recompute and update the map tile edge and position, based on
  the given window width+height and game map dimensions"
  (setf *tile-edge* (get-tile-edge width height map))
  (setf *draw-start* (get-draw-start width height map)))

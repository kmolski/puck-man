(defconstant +time-to-start+ 10)
(defconstant +default-lives+ 3)

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
   (timer )
   (timer-start )
   (time-at-pause )
   (stage :initform 'init)))

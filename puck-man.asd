(asdf:defsystem #:puck-man
  :depends-on (#:sdl2 #:sdl2-image #:trivia)
  :components ((:file "window")
               (:file "map" :depends-on ("window"))
               (:file "game-state" :depends-on ("window" "map"))))

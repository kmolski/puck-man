(asdf:defsystem #:puck-man
  :depends-on (#:sdl2 #:trivia)
  :components ((:file "game-map")
               (:file "game-state" :depends-on ("game-map"))))

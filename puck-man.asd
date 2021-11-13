(asdf:defsystem #:puck-man
  :depends-on (#:sdl2 #:trivia)
  :components ((:file "map")
               (:file "game-state" :depends-on ("map"))))

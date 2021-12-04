(asdf:defsystem #:puck-man
  :depends-on (#:sdl2 #:sdl2-image #:trivia)
  :components ((:file "window")
               (:file "map" :depends-on ("window"))
               (:file "entity")
               (:file "ghost" :depends-on ("entity" "window"))
               (:file "player" :depends-on ("entity" "window"))
               (:file "tracking-strategy" :depends-on ("ghost"))
               (:file "game-state" :depends-on ("ghost" "map" "player" "tracking-strategy" "window"))))

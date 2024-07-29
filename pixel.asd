(defsystem "sdl2-tutorial"
  :description "SDL2 Learning"
  :version "0.0.1"
  :author "somni <rosin@protonmail.com>"
  :licence "Public Domain"
  :depends-on ("bordeaux-threads" "sdl2" "sdl2-image" "sdl2-ttf")
  :components ((:file "scratch"))
  ;:in-order-to ((test-op (test-op "sdl2-tutorial/tests")))
  )


; (defsystem "sdl2-tutorial/tests"
;   :depends-on ("parachute")
;   :components ((:file "tests/tests"))
;   :perform (test-op (op c) (symbol-call :parachute :test :sdl2-tutorial-tests)))

; C-c C-c
(defpackage #:scratch
  (:use :cl))

; C-c C-j
(in-package :scratch)

; Why does this make me tear up?
(ql:quickload '(:bordeaux-threads :sdl2 :sdl2-image :sdl2-ttf :sdl2-mixer))

; Variables
(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

; Ambiguity is interesting -- because you haven't landed -- you're still in the air
;  -- you haven't alighted anywhere -- it's good buddhist practice
(defmacro with-window-surface ((window surface &key (name "PIXEL") 
                                                    (width *screen-width*) 
                                                    (height *screen-height*)) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title ,name
                        :w ,width
                        :h ,height
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         ,@body))))

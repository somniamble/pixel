(defpackage #:scratch
  (:use :cl))

(in-package :scratch)

(ql:quickload '(:bordeaux-threads :sdl2 :sdl2-image :sdl2-ttf))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)


; (sdl2:with-init (:video)
;   (sdl2:with-window (window :title "PIXEL PUSHER"
;                             :w 640
;                             :h 480
;                             :flags '(:show))
;     (let ((screen-surface (sdl2:get-window-surface window)))
;       (sdl2:fill-rect screen-surface
;                       nil
;                       (sdl2:map-rgb (sdl2:surface-format screen-surface) 255 0 0))
;       (sdl2:update-window window)
;       (sdl2:delay 2000))))

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
                        
(defun run()
  (with-window-surface (window surface)
    (sdl2:show-window window)
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:keydown (:keysym keysym)
       (case (sdl2:scancode keysym)
         (:scancode-escape (sdl2:quit))
         (t :default)))
      (:idle ()
             (sdl2:fill-rect surface
                    nil
                    (sdl2:map-rgb (sdl2:surface-format surface) 
                                  (random 255) 
                                  (random 255) 
                                  (random 255)))
             (sdl2:update-window window)
             (sdl2:delay 100)))))

(run)
                      
    ; (sdl2:with-init (:video)
    ;   (sdl2:with-window (window :title "PIXEL PUSHER"
    ;                             :w 640
    ;                             :h 480
    ;                             :flags '(:show))
    ;     (let ((screen-surface (sdl2:get-window-surface window)))
    ;       (sdl2:fill-rect screen-surface
    ;                       nil
    ;                       (sdl2:map-rgb (sdl2:surface-format screen-surface) 255 0 0))
    ;       (sdl2:update-window window)
    ;       (sdl2:delay 2000))))

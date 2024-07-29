(defpackage #:scratch
  (:use :cl))

(in-package :scratch)

(ql:quickload '(:bordeaux-threads :sdl2 :sdl2-image :sdl2-ttf))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

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

(defparameter *red* 128)
(defparameter *green* 128)
(defparameter *blue* 128)
(defparameter *update-red* t)
(defparameter *update-green* t)
(defparameter *update-blue* t)

(defun random-walk (start max-down max-up)
  (+ start (- (random (+ max-down max-up 1)) max-down)))

(defun update-red ()
  (let* ((max-up (min 10 (- 255 *red*)))
         (max-down (min 10 *red*))
         (updated (random-walk *red* max-down max-up)))
  (if *update-red* (setf *red* updated))))

(defun update-green ()
  (let* ((max-up (min 10 (- 255 *green*)))
         (max-down (min 10 *green*))
         (updated (random-walk *green* max-down max-up)))
    (if *update-green* (setf *green* updated))))

(defun update-blue ()
  (let* ((max-up (min 10 (- 255 *blue*)))
         (max-down (min 10 *blue*))
         (updated (random-walk *blue* max-down max-up)))
    (if *update-blue* (setf *blue* updated))))
                        
(defun run()
  (with-window-surface (window surface)
    (sdl2:show-window window)
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:keydown (:keysym keysym)
       (format t "pressing: ~a" keysym)
       (case (sdl2:scancode keysym)
         (:scancode-escape (sdl2:quit))
         (:scancode-r (setf *update-red* (not *update-red*)))
         (:scancode-g (setf *update-green* (not *update-green*)))
         (:scancode-b (setf *update-blue* (not *update-blue*)))
         (t :default)))
      (:idle ()
             (update-red)
             (update-green)
             (update-blue)
             (sdl2:fill-rect surface
                    nil
                    (sdl2:map-rgb (sdl2:surface-format surface) 
                                  *red*
                                  *green*
                                  *blue*))
             (sdl2:update-window window)
             (sdl2:delay 16)))))

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

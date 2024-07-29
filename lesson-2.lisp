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

;; Include an event-loop handler that will let us exit safely
(defmacro with-harness ((window surface &key (name "PIXEL")
                                             (events '())
                                             (after '())
                                             (width *screen-width*) 
                                             (height *screen-height*)) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title ,name
                        :w ,width
                        :h ,height
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         (sdl2:show-window ,window)
         (sdl2:with-event-loop (:method :poll)
           (:quit () t)
           (:keydown (:keysym keysym)
            (format t "pressing ~a" keysym)
            (case (sdl2:scancode keysym)
              (:scancode-escape (sdl2:push-event :quit))
              ,@events
              (t :default)))
           (:idle () ,@body))
         ,@after))))


 ; (sdl2:with-event-loop (:method :poll)
 ;   (:quit () t)
 ;   (:keydown (:keysym keysym)
 ;    (format t "pressing ~a" keysym)
 ;    (case (sdl2:scancode keysym)
 ;      (:scancode-escape (sdl2:push-event :quit))
 ;      (:scancode-r (setf ctx (initialize-context)))
 ;      (:scancode-t (setf (gethash :toggle ctx) (not (gethash :toggle ctx))))
 ;      (t :default)))
 ;   (:idle ()
 ;    (handle ctx my-window my-surface)))))

; pretty cool idea, but we repeatedly load this image every "frame"
; so, what if we pass down a context?


(defun get-ctx (item ctx)
  (gethash item ctx))

(defun handle (context window surface)
  (sdl2:blit-surface
   (if (get-ctx :toggle context) 
       (get-ctx :self-image context) 
       (get-ctx :self-image-v3 context))
       NIL surface NIL)
  ;(sdl2:blit-surface
  ; (sdl2-image:load-image "selfv3.png") NIL surface NIL)
  (sdl2:update-window window)
   (sdl2:delay 16))

; very simple state management idea
(defun initialize-context ()
  (let ((table (make-hash-table)))
    (setf (gethash :toggle table) t)
    (setf (gethash :self-image table) 
          (sdl2-image:load-image "images/self.png"))
    (setf (gethash :self-image-v3 table) 
          (sdl2-image:load-image "images/selfv3.png"))
    table))

(defun free-context (ctx)
  (sdl2:free-surface (get-ctx :self-image ctx))
  (sdl2:free-surface (get-ctx :self-image-v3 ctx)))

(with-window-surface (my-window my-surface)
  (let ((ctx (initialize-context)))
  (sdl2:show-window my-window)
  (sdl2:with-event-loop (:method :poll)
    (:quit () t)
    (:keydown (:keysym keysym)
     (format t "pressing ~a" keysym)
     (case (sdl2:scancode keysym)
       (:scancode-escape (sdl2:push-event :quit))
       (:scancode-r (setf ctx (initialize-context)))
       (:scancode-t (setf (gethash :toggle ctx) (not (gethash :toggle ctx))))
       (t :default)))
    (:idle ()
     (handle ctx my-window my-surface)))))
       

(let ((ctx (initialize-context)))
  (with-harness (window surface
                        :after ((free-context ctx))
                        :events
                        ((:scancode-r (progn (free-context ctx) (setf ctx (initialize-context))))
                         (:scancode-t (setf (gethash :toggle ctx) (not (gethash :toggle ctx))))))
                        (handle ctx window surface)))

;; how do I draw an image?

; (with-window-surface (my-window my-surface)
;   (let ((ctx (initialize-context)))
;     (sdl2:show-window my-window)
;     (sdl2:with-event-loop (:method :poll)
;       (:quit () t)
;       (:keydown (:keysym keysym)
;        (format t "pressing ~a" keysym)
;        (case (sdl2:scancode keysym)
;          (:scancode-escape (sdl2:push-event :quit))
;          (:scancode-r (setf ctx (initialize-context)))
;          (t :default)))
;       (:idle ()
;        (handle ctx my-window my-surface)))))

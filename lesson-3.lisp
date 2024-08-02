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

;; (declaim (ftype (function (fixnum) fixnum) add))
;; ;;                         ^^input ^^output [optional]

(declaim (ftype (function (string sdl2-ffi:sdl-surface) sdl2-ffi:sdl-surface) load-optimized-image))

(defun load-optimized-image (filename screen)
  "Loads an image, converted to the format of the screen"
  (let* ((img (sdl2-image:load-image filename))
         (converted (sdl2:convert-surface img (sdl2:surface-format screen))))
    (sdl2:free-surface img)
    converted))
    

; very simple state management idea
(defun initialize-context (window screen)
  (declare (ignorable window screen))
  (let ((table (make-hash-table)))
    (setf (gethash :toggle table) t)
    (setf (gethash :self-image table) 
          (load-optimized-image "images/self.png" screen))
    (setf (gethash :self-image-v3 table) 
          (load-optimized-image "images/selfv3.png" screen))
    table))
(defun initialize-context (window screen)
  (declare (ignorable window screen))
  (let ((table (make-hash-table)))
    (setf (gethash :toggle table) t)
    (setf (gethash :self-image table) 
          (load-optimized-image "images/self.png" screen))
    (setf (gethash :self-image-v3 table) 
          (load-optimized-image "images/selfv3.png" screen))
    table))

(defun free-it (item)
  (typecase item
    (sdl2-ffi:sdl-surface (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:free-surface item))
    (sdl2-ffi:sdl-rect (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:free-rect item))
    (sdl2-ffi:sdl-point (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:free-point item))
    (t (format t "not going to free a ~(~A~)" (type-of item)))))

(defun free-context (ctx)
  (loop for item being the hash-values in ctx
        do (free-it item)))

(defun noop (&rest args)
  (declare (ignore args)))

; Ambiguity is interesting -- because you haven't landed -- you're still in the air
;  -- you haven't alighted anywhere -- it's good buddhist practice

;; Include an event-loop handler that will let us exit safely
(defmacro with-harness ((window surface context &key (name "PIXEL")
                                             (keys '())
                                             (ctx-construct #'noop)
                                             (ctx-teardown #'noop)
                                             (after '())
                                             (width *screen-width*) 
                                             (height *screen-height*)) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title ,name
                        :w ,width
                        :h ,height
                        :flags '(:shown))
       (let* ((,surface (sdl2:get-window-surface ,window))
              (,context (funcall ,ctx-construct ,window ,surface)))
         (sdl2:show-window ,window)
         (sdl2:with-event-loop (:method :poll)
           (:quit () t)
           (:keydown (:keysym keysym)
            (format t "pressing ~a" keysym)
            (case (sdl2:scancode keysym)
              (:scancode-escape (sdl2:push-event :quit))
              ,@keys
              (t :default)))
           (:idle () ,@body))
         ,@after
         (funcall ,ctx-teardown ,context)))))


; (with-harness window screen ctx
;   :keys
;   ((:scancode-r (progn (free-context ctx) (setf ctx (initialize-context))))
;    (:scancode-t (setf (gethash :toggle ctx) (not (gethash :toggle ctx)))))
; (handle ctx window surface))
  

(defun refresh-context (window screen context)
  (free-context context)
  (initialize-context window screen))

(with-harness (wndw scrn ctx
                    :keys ((:scancode-f (princ "foo"))
                           (:scancode-r (setf ctx (refresh-context wndw scrn ctx))))
                    :ctx-construct #'initialize-context
                    :ctx-teardown #'free-context)
  (handle ctx wndw scrn))

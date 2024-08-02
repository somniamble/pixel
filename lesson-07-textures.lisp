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

;; (declaim (ftype (function (fixnum) fixnum) add))
;; ;;                         ^^input ^^output [optional]

; (declaim (ftype (function (string sdl2-ffi:sdl-surface) sdl2-ffi:sdl-surface) load-optimized-image))
; 
(defun load-image (filename renderer)
  "Loads an image, converted to the format of the screen"
  (let* ((img (sdl2-image:load-image filename)) ; image surface
         (texture (sdl2:create-texture-from-surface renderer img)))
    (sdl2:free-surface img)
    texture))
    

(defvar *game-context* (make-hash-table))
; very simple state management idea
(defun initialize-context (window screen renderer)
  (declare (ignorable window screen renderer))
  (let ((table (make-hash-table)))
    (setf (gethash :lock-mouse table) nil)
    (setf (gethash :toggle table) t)
    (setf (gethash :x table) (/ *screen-width* 2))
    (setf (gethash :y table) (/ *screen-height* 2))
    (setf (gethash :self-image table) 
          (load-image "images/self.png" renderer))
    (setf (gethash :self-image-v3 table) 
          (load-image "images/selfv3.png" renderer))
    (setf *game-context* table)
    table))

(defun free-it (item)
  (typecase item
    (sdl2-ffi:sdl-surface (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:free-surface item))
    (sdl2-ffi:sdl-rect (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:free-rect item))
    (sdl2-ffi:sdl-point (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:free-point item))
    (sdl2-ffi:sdl-renderer  (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:destroy-renderer item))
    (sdl2-ffi:sdl-texture  (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:destroy-texture item))
    (t (format t "not going to free a ~(~A~)" (type-of item)))))

(defun free-context (ctx)
  (loop for item being the hash-values in ctx
        do (free-it item)))

(defun noop (&rest args)
  (declare (ignore args)))

; Ambiguity is interesting -- because you haven't landed -- you're still in the air
;  -- you haven't alighted anywhere -- it's good buddhist practice

; ;
; :YREL (SDL2-FFI:SINT32)
; :XREL (SDL2-FFI:SINT32)
; :Y (SDL2-FFI:SINT32)
; :X (SDL2-FFI:SINT32)
; :STATE (SDL2-FFI:UINT32)
; :WHICH (SDL2-FFI:UINT32)
; :WINDOW-ID (SDL2-FFI:UINT32)
; :TIMESTAMP (SDL2-FFI:UINT32)
; :TYPE (SDL2-FFI:UINT32)

;; Include an event-loop handler that will let us exit safely
(defmacro with-harness ((window surface renderer context &key (name "PIXEL")
                                                              (keys '())
                                                              (events '())
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
       (sdl2:with-renderer (,renderer ,window)
         (let* ((,surface (sdl2:get-window-surface ,window))
                (,context (funcall ,ctx-construct ,window ,surface ,renderer)))
           (sdl2:show-window ,window)
           (sdl2:with-event-loop (:method :poll)
             (:quit () t)
             (:keydown (:keysym keysym)
              (format t "pressing ~a" keysym)
              (case (sdl2:scancode keysym)
                (:scancode-escape (sdl2:push-event :quit))
                ,@keys
                (t :default)))
             ,@events
             (:idle () ,@body))
           ,@after
           (funcall ,ctx-teardown ,context))))))


; (with-harness window screen renderer
;   :keys
;   ((:scancode-r (progn (free-context ctx) (setf ctx (initialize-context))))
;    (:scancode-t (setf (gethash :toggle ctx) (not (gethash :toggle ctx)))))
; (handle ctx window surface renderer))
  

(defun refresh-context (window screen renderer context)
  (free-context context)
  (initialize-context window screen renderer))

(ql:quickload :cl-hash-table-destructuring)

(cl-hash-table-destructuring:with-hash-table-values
    ((toggle :toggle) (x :x) (y :y)) *game-context*
  (princ toggle)
  (princ x)
  (princ y))

; (defun mouse-handler (ctx x y &rest rest)
;   (declare (ignore rest))
;   (when (sdl2:mouse-state-p 1)
;     (setf (gethash :lock-mouse ctx) (not (gethash :lock-mouse ctx))))
;   (unless (gethash :lock-mouse ctx)
;     (setf (gethash :x ctx) x)
;     (setf (gethash :y ctx) y)))

(defun mouse-handler (ctx x y &rest rest)
  (declare (ignore rest))
  (when (sdl2:mouse-state-p 1)
    (setf (gethash :x ctx) x)
    (setf (gethash :y ctx) y)))

(defun handle (context window surface renderer)
  (declare (ignorable context window surface renderer))
  (cl-hash-table-destructuring:with-hash-table-values
      ((toggle :toggle) 
       (x :x) 
       (y :y)
       (gpoy-frown :self-image)
       (gpoy-smile :self-image-v3)) context
    (sdl2:with-rects ((q1 0 0 x y)
                      (q2 x 0 (- *screen-width* x) y)
                      (q3 0 y x (- *screen-height* y))
                      (q4 x y (- *screen-width* x) (- *screen-height* y)))
      ; (sdl2:update-window window)
      (sdl2:render-clear renderer)
      ; draw q1
      (sdl2:render-copy renderer 
                        (if toggle 
                            gpoy-frown
                            gpoy-smile)
                        :dest-rect q1)
      (sdl2:render-copy renderer
                        (if toggle
                            gpoy-smile
                            gpoy-frown)
                        :dest-rect q2)
      (sdl2:render-copy renderer 
                        (if toggle 
                            gpoy-smile
                            gpoy-frown)
                        :dest-rect q3)
      (sdl2:render-copy renderer
                        (if toggle
                            gpoy-frown
                            gpoy-smile)
                        :dest-rect q4)
      (sdl2:render-present renderer)
      (sdl2:delay 10))))

;                //Render red filled quad
; SDL_Rect fillRect = { SCREEN_WIDTH / 4, SCREEN_HEIGHT / 4, SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2 };
; SDL_SetRenderDrawColor( gRenderer, 0xFF, 0x00, 0x00, 0xFF );        
; SDL_RenderFillRect( gRenderer, &fillRect );
; we could also do RenderDrawRect instead
;
(with-harness (wndw scrn rndr ctx
                    :keys ((:scancode-f (princ "foo"))
                           (:scancode-t (setf (gethash :toggle ctx) (not (gethash :toggle ctx))))
                           (:scancode-r (setf ctx (refresh-context wndw scrn rndr ctx))))
                    :events ((:mousemotion (:x x :y y)
                               (mouse-handler ctx x y)))
                    :ctx-construct #'initialize-context
                    :ctx-teardown #'free-context)
  (handle ctx wndw scrn rndr))


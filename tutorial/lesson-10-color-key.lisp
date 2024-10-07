
; C-c C-c
(defpackage #:scratch/lesson-10-color-key
  (:use :cl :scratch/harness :scratch/drawable))

; C-c C-j
(in-package :scratch/lesson-10-color-key)
    
(defun initialize-context (window screen renderer)
  (declare (ignorable window screen renderer))
  (let ((table (make-hash-table)))
    (setf (gethash :lock-mouse table) nil)
    (setf (gethash :toggle table) t)
    (setf (gethash :x table) (/ *screen-width* 2))
    (setf (gethash :y table) (/ *screen-height* 2))
    (setf (gethash :darksouls table)
          (load-image "images/darksouls.png" renderer :color-key (make-rgb :r 255 :g 255 :b 255)))
    (setf (gethash :background table)
          (load-image "images/background.png" renderer))
    (setf (gethash :rgb table)
          (load-image "images/rgb.bmp" renderer))
    ;;     (setf (gethash :self-image table) 
    ;;           (load-image "images/self.png" renderer))
    ;;     (setf (gethash :self-image-v3 table) 
    ;;           (load-image "images/selfv3.png" renderer))
    (setf *game-context* table)
    table))

(defun free-context (ctx)
  (loop for item being the hash-values in ctx
        do (free-it item)))

(defun refresh-context (window screen renderer context)
  (free-context context)
  (initialize-context window screen renderer))

(defun mouse-handler (ctx x y &rest rest)
  (declare (ignore rest))
  (when (sdl2:mouse-state-p 1)
    (setf (gethash :x ctx) x)
    (setf (gethash :y ctx) y)))

(defun draw-box (renderer rect r g b a)
  (sdl2:set-render-draw-color renderer r g b a)
  (sdl2:render-fill-rect renderer rect)
  (sdl2:set-render-draw-color renderer 0 0 0 a)
  (sdl2:render-draw-line renderer 
                         (sdl2:rect-x rect)
                         (+ (sdl2:rect-y rect) (sdl2:rect-height rect))
                         (+ (sdl2:rect-x rect) (sdl2:rect-width rect))
                         (sdl2:rect-y rect)))

(defun handle (context window surface renderer)
  (declare (ignorable context window surface renderer))
  (cl-hash-table-destructuring:with-hash-table-values
      ((toggle :toggle) 
       (x :x) 
       (y :y)
       (darksouls :darksouls)
       (background :background)) context
    (sdl2:set-render-draw-color renderer 255 255 255 255)
    (draw background 0 0)
    (draw darksouls x y)
    (sdl2:render-present renderer)
    (sdl2:delay 10)))
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
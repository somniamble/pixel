(defpackage #:scratch/drawable
  (:use :cl)
  (:export #:draw
           #:free
           #:drawable
           #:texture
           #:make-rgb
           #:load-image))

(in-package :scratch/drawable)

(defclass drawable () 
  ((renderer :initarg :renderer 
              :reader renderer)))

;; a no-op, by default
(defmethod draw ((x drawable) pos-x pos-y))

(defmethod free ((x drawable)))

(defclass box (drawable) ())

(defclass texture (drawable) 
  ((texture :initarg :texture
            :reader texture)
   (pixel-format :initarg :pixel-format
                 :reader pixel-format)
   (width :initarg :width
          :reader width)
   (height :initarg :height
           :reader height)))

(defmethod draw ((tx texture) pos-x pos-y)
  (sdl2:with-rects ((r pos-x pos-y (width tx) (height tx)))
    (sdl2:render-copy (renderer tx) (texture tx) :dest-rect r)))

(defmethod free ((tx texture))
  (sdl2:destroy-texture (texture tx)))

(defstruct rgb 
  (r 0 :type integer)
  (g 0 :type integer)
  (b 0 :type integer))

(defun load-image (filename renderer &key (color-key nil))
  "Loads an image, converted to the format of the screen"
  (let* ((img (sdl2-image:load-image filename)) ; image surface
         (pxl-fmt (sdl2:surface-format img)))
    (when color-key
      (sdl2:set-color-key img sdl2-ffi:+true+ (sdl2:map-rgb pxl-fmt 
                                                            (rgb-r color-key)
                                                            (rgb-g color-key)
                                                            (rgb-b color-key))))
    (let ((texture (sdl2:create-texture-from-surface renderer img)))
      (sdl2:free-surface img)
      (make-instance 'texture
                     :height (sdl2:texture-height texture)
                     :width (sdl2:texture-width texture)
                     :texture texture
                     :pixel-format pxl-fmt
                     :renderer renderer))))

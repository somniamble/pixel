(defpackage #:scratch/harness
  (:use :cl)
  (:export #:*screen-width*
           #:*screen-height*
           #:*game-context*
           #:free-it
           #:noop
           #:with-harness))

(in-package :scratch/harness)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defvar *game-context* (make-hash-table))

(defun free-it (item)
  (typecase item
    (sdl2-ffi:sdl-surface (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:free-surface item))
    (sdl2-ffi:sdl-rect (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:free-rect item))
    (sdl2-ffi:sdl-point (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:free-point item))
    (sdl2-ffi:sdl-renderer  (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:destroy-renderer item))
    (sdl2-ffi:sdl-texture  (format t "Freeing a ~(~A~)" (type-of item)) (sdl2:destroy-texture item))
    (scratch/drawable:drawable (format t "Freeing a ~(~A~)" (type-of item)) (scratch/drawable:free item))
    (t (format t "not going to free a ~(~A~)" (type-of item)))))

(defun noop (&rest args)
  (declare (ignore args)))

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

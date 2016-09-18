(in-package :inferior-shell)

;;--- TODO: move these to FARE-UTILS ?

(defmacro with-directory ((dir) &body body)
  `(call-with-directory ,dir (lambda () ,@body)))

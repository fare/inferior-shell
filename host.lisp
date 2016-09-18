(in-package :inferior-shell)

(defparameter *current-host-names* '("localhost"))

(defun current-host-name-p (x)
  (and (stringp x) (member x *current-host-names* :test 'equal)))

(defun initialize-current-host-names ()
  (setf *current-host-names*
        `("localhost"
          ,(run/ss '(hostname -f))
          ,(run/ss '(hostname -s)))))

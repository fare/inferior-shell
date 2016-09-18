(in-package :inferior-shell)

;;--- TODO: move these to FARE-UTILS and/or XCVB-DRIVER ?

(defun make-directory (dir &optional (mode #o755))
  ;; TODO: move to fare-utils or somewhere else
  ;; TODO: it's low-level. rename this mkdir or some such, and document use of native-namestring.
  #+clozure (ccl::%mkdir dir mode)
  #+sbcl (sb-posix:mkdir dir mode)
  #-(or clozure sbcl) (NIY 'make-directory))

(defun setenv (var val &optional (overwritep t))
  #+clozure (ccl:setenv var val overwritep)
  #+sbcl (sb-posix:setenv var val (if overwritep 1 0))
  #-(or clozure sbcl) (NIY 'setenv))

(defmacro pipe (values &rest transformers)
  (if (null transformers)
      values
      `(pipe (multiple-value-call ,(car transformers) ,values)
             ,@(cdr transformers))))

(defun split-lines (x)
  (split-string x :separator #(#\newline)))

(defun split-lines* (x)
  (remove-if #'emptyp (split-lines x)))

(defun select-from-hash (keys hash)
  (loop :for key :in keys
    :collect (gethash key hash)))

(defun do-stream-lines (fun stream)
  (loop :for line = (read-line stream nil) :while line
    :do (funcall fun line)))

(defun do-string-lines (fun string)
  (with-input-from-string (stream string)
    (do-stream-lines fun stream)))

(defun read-line* (&optional (stream *standard-input*) eof-error-p eof-value recursive-p cr lf)
  "Similar to READ-LINE, this function also returns as additional values the state about
whether CR or LF were read. CR, LF and CR+LF are accepted only.
Partial state accepted as input, too, for parsing in chunks."
  (loop
    :with eof = '#:eof
    :with datap = nil
    :with out = (make-string-output-stream)
    :for char = (read-char stream nil eof recursive-p) :do
      (labels ((out (c) (setf datap t) (write-char c out))
               (unpeek () (unread-char char stream))
               (done () (return (values (get-output-stream-string out) cr lf)))
               (unpeek-done () (unpeek) (done)))
        (cond
          ((eql char #\newline)
           (if lf (unpeek-done) (setf lf t)))
          ((eql char #\cr)
           (if (or cr lf) (unpeek-done) (setf cr t)))
          ((eql char eof)
           (cond
             (eof-error-p (read-char stream eof-error-p eof-value recursive-p))
             (datap (done))
             (t (return (values eof-value nil nil)))))
          (t
           (out char))))))

(defun add-days (year month date days)
  (multiple-value-bind (sec min hr d m y dlsp tz)
      (decode-universal-time
       (+ (encode-universal-time 0 0 0 date month year 0) (* 60 60 24 days)) 0)
    (declare (ignore sec min hr dlsp tz))
    (values y m d)))

(defun previous-day (year month date)
  (add-days year month date -1))

(defun next-day (year month date)
  (add-days year month date 1))

(defun zglobcmd (&rest patterns)
  `(zsh --nullglob -fc ("print -l " ,(join-strings patterns :separator " "))))

(defun zglob (patterns &key host)
  (run/lines `((> 2 "/dev/null") ,@(apply 'zglobcmd (alexandria:ensure-list patterns)))
             :host host))

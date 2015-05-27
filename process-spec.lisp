#+xcvb (module (:depends-on ("pkgdcl")))

(cl:in-package :inferior-shell)

(in-readtable :fare-quasiquote)

(defclass process-spec (simple-print-object-mixin) ())

(defclass sequence-spec (process-spec)
  ((processes
    :type list
    :initarg :processes :reader sequence-processes)))

(defclass pipe-spec (sequence-spec) ())

(defclass or-spec (sequence-spec) ())

(defclass and-spec (sequence-spec) ())

(defclass progn-spec (sequence-spec) ())

(defclass fork-spec (sequence-spec) ())

(defclass redirection (simple-print-object-mixin) ())

(defclass file-redirection (redirection)
  ((fd
    :type integer
    :initarg :fd :reader redirection-fd)
   (symbol ;; shell symbol
    :type symbol
    :initarg :symbol :reader redirection-symbol)
   (flags ;; direction and flags for cl:open
    :type list
    :initarg :flags :reader redirection-flags)
   (pathname
    :type (or string pathname)
    :initarg :pathname :reader redirection-pathname)))

(defun check-small-fd (fd)
  (unless (typep fd '(integer 0 9))
    (error "Can only do redirections with fd between 0 and 9")))

(defclass fd-redirection (redirection)
  ;; when you'd tell a shell $new >& $old,
  ;; you mean the C code dup2(old,new)
  ;; The new descriptor is copied from the old one
  ;; (any previously bound value is closed).
  ((old-fd
    :type integer
    :initarg :old-fd :reader redirection-old-fd)
   (new-fd
    :type integer
    :initarg :new-fd :reader redirection-new-fd)))

(defclass close-redirection (redirection)
  ;; close a fd
  ((old-fd
    :type integer
    :initarg :old-fd :reader redirection-old-fd)))

(defun make-file-redirection (symbol fd pn flags)
  (make-instance 'file-redirection
                 :symbol symbol :fd fd :pathname pn :flags flags))

(defun make-fd-redirection (old-fd new-fd)
  (make-instance 'fd-redirection :old-fd old-fd :new-fd new-fd))

(defun make-close-redirection (old-fd)
  (make-instance 'close-redirection :old-fd old-fd))

(defgeneric print-process-spec (r &optional s)
  (:documentation "Print a process specification in a way suitable for consumption by a shell"))

(defmethod print-process-spec ((r file-redirection) &optional s)
  (with-output (s)
    (with-slots (fd symbol pathname) r
      (when (eq symbol '!)
        (error "Can't print ad-hoc redirection ~S" r))
      (unless (= fd (case symbol ((< <>) 0) (otherwise 1)))
        (check-small-fd fd)
        (format s "~D" fd))
      (format s "~A " symbol)
      (escape-command (list pathname) s))))

(defmethod print-process-spec ((r fd-redirection) &optional s)
  (with-output (s)
    (with-slots (new-fd old-fd) r
      (check-small-fd old-fd)
      (check-small-fd new-fd)
      (case new-fd
        (0 (format s "<& ~D" old-fd))
        (1 (format s ">& ~D" old-fd))
        (otherwise (format s "~D>& ~D" new-fd old-fd))))))

(defmethod print-process-spec ((r close-redirection) &optional s)
  (with-output (s)
    (with-slots (old-fd) r
      (check-small-fd old-fd)
      (case old-fd
        (0 (princ "<& -" s))
        (1 (princ ">& -" s))
        (otherwise (format s "~D>& -" old-fd))))))

(defclass command-spec (process-spec)
  ((arguments
    :type list
    :initarg :arguments :reader command-arguments)
   (redirections
    :type list
    :initarg :redirections :reader command-redirections)))

(defclass command-parse ()
  ((arguments-r
    :type list :initform nil
    :accessor clps-arguments-r)
   (redirections-r
    :type list :initform nil
    :accessor clps-redirections-r)
   (current-argument
    :type (or null stream) :initform nil
    :accessor clps-current-argument)))

(defgeneric ensure-argument (command-parse))
(defgeneric flush-argument (command-parse))
(defgeneric new-argument (command-parse))
(defgeneric extend-argument (command-parse string))
(defgeneric add-argument (command-parse string))
(defgeneric add-redirection (command-parse redirection))
(defgeneric command-parse-results (command-parse))

(defmethod ensure-argument ((c command-parse))
  (unless (clps-current-argument c)
    (setf (clps-current-argument c) (make-string-output-stream)))
  (values))

(defmethod flush-argument ((c command-parse))
  (with-accessors ((current clps-current-argument)
                   (arguments clps-arguments-r)) c
    (when current
      (push (get-output-stream-string current) arguments)
      (close current)
      (setf current nil)))
  (values))

(defmethod new-argument ((c command-parse))
  (flush-argument c)
  (ensure-argument c)
  (values))

(defmethod extend-argument ((c command-parse) x)
  (ensure-argument c)
  (princ x (clps-current-argument c))
  (values))

(defmethod add-argument ((c command-parse) (argument string))
  (flush-argument c)
  (push argument (clps-arguments-r c))
  (values))

(defmethod add-redirection ((c command-parse) redirection)
  (push redirection (clps-redirections-r c))
  (values))

(defmethod command-parse-results ((c command-parse))
  (flush-argument c)
  (prog1
      (make-instance
       'command-spec
       :arguments (nreverse (clps-arguments-r c))
       :redirections (nreverse (clps-redirections-r c)))
    (setf (clps-arguments-r c) nil
          (clps-redirections-r c) nil
          (clps-current-argument c) nil)))

(defun parse-process-spec (spec)
  (labels ((make-sequence-instance (spec-type args)
             (make-instance spec-type :processes
              (loop :for process :in (mapcar 'parse-process-spec args)
                    :nconc (if (typep process spec-type)
                               (sequence-processes process)
                               (list process))))))
    (match spec
      (`(pipe ,@args)
        (make-sequence-instance 'pipe-spec args))
      (`(or ,@args)
        (make-sequence-instance 'or-spec args))
      (`(and ,@args)
        (make-sequence-instance 'and-spec args))
      (`(progn ,@args)
        (make-sequence-instance 'progn-spec args))
      (`(fork ,@args)
        (make-sequence-instance 'fork-spec args))
      (`(,_ ,@_)
        (let ((c (make-instance 'command-parse)))
          (dolist (elem spec)
            (parse-command-spec-top-token c elem))
          (command-parse-results c)))
      (_
       (error "Invalid process spec ~S" spec)))))

(deftype simple-command-line-token () '(or string pathname keyword symbol character integer))

(defun token-string (x)
  (with-safe-io-syntax ()
    (typecase x
      (null "")
      (character (format nil "-~A" x))
      (keyword (format nil "--~(~A~)" x))
      (symbol (string-downcase x))
      (string x)
      (pathname (native-namestring (translate-logical-pathname x)))
      (cons (with-output-to-string (s) (mapcar (curry 'write-token-component s) x)))
      (t (princ-to-string x)))))

(defun write-token-component (s x)
  (typecase x
    (null nil)
    (character (write-char x s))
    (symbol (write-string (string-downcase x) s))
    (string (write-string x s))
    (pathname (write-string (native-namestring (translate-logical-pathname x)) s))
    (cons (mapcar (curry 'write-token-component s) x))
    (t (princ x s))))

(defun parse-command-spec-top-token (c x)
  (labels
      ((r (x)
         (add-redirection c x))
       (f (sym fd pn flags)
         (r (make-file-redirection sym fd (token-string pn) flags)))
       (fd (old new)
         (r (make-fd-redirection old new)))
       (cl (old)
         (r (make-close-redirection old)))
       (c (x)
         (match x
           (`(! ,fd ,pn ,@flags) (c `(:open ,fd ,pn ,@flags)))
           (`(:open ,fd ,pn ,@flags) (f '! fd pn flags))

           (`(- ,fd) (c `(:close ,fd)))
           (`(:close ,fd) (cl fd))

           (`(< ,@args) (c `(:input ,@args)))
           (`(:input ,pn) (c `(:input 0 ,pn)))
           (`(:input ,fd ,pn)
             (f '< fd pn
                '(:input :if-does-not-exist :error)))

           (`(<> ,@args) (c `(:io ,@args)))
           (`(:io ,pn) (c `(:io 0 ,pn)))
           (`(:io ,fd ,pn)
             (f '<> fd pn
                '(:io :if-exists :overwrite :if-does-not-exist :error)))

           (`(> ,@args) (c `(:output ,@args)))
           (`(:output ,pn) (c `(:output 1 ,pn)))
           (`(:output ,fd ,pn)
             (f '> fd pn
                '(:output :if-exists :error :if-does-not-exist :create)))

           (`(:error-output ,pn) (c `(:output 2 ,pn)))


           (`(>! ,@args) (c `(:force-output ,@args)))
           (`(:force-output ,pn) (c `(:force-output 1 ,pn)))
           (`(:force-output ,fd ,pn)
             (f '>! fd pn
                '(:output :if-exists :supersede :if-does-not-exist :create)))

           (`(>> ,@args) (c `(:append ,@args)))
           (`(:append ,pn) (c `(:append 1 ,pn)))
           (`(:append ,fd ,pn)
             (f '>> fd pn
                '(:output :if-exists :append :if-does-not-exist :error)))

           (`(>>! ,@args) (c `(:force-append ,@args)))
           (`(:force-append ,pn) (c `(:append 1 ,pn)))
           (`(:force-append ,fd ,pn)
             (f '>>! fd pn
                '(:output :if-exists :append :if-does-not-exist :create)))

           (`(<& ,@args) (c `(:redirect-input ,@args)))
           (`(:redirect-input ,old-fd -)
             (cl old-fd))
           (`(:redirect-input ,new-fd ,old-fd)
             (fd old-fd new-fd))
           (`(:redirect-input -)
             (cl 0))

           (`(>& ,@args) (c `(:redirect-output ,@args)))
           (`(:redirect-output ,old-fd -)
             (cl old-fd))
           (`(:redirect-output ,new-fd ,old-fd)
             (fd old-fd new-fd))
           (`(:redirect-output -)
             (cl 1))
           (`(:redirect-output ,pn)
             (c `(:output 1 ,pn))
             (c `(:redirect-output 2 1)))

           (`(>&! ,pn) (c `(:force-redirect ,pn)))
           (`(:force-redirect ,pn)
             (c `(:force-output 1 ,pn))
             (c `(:redirect-output 2 1)))

           (`(>>& ,pn) (c `(:redirect-append ,pn)))
           (`(:redirect-append ,pn)
             (c `(>> 1 ,pn))
             (c `(>& 2 1)))

           (`(>>&! ,pn) (c `(:force-redirect-append ,pn)))
           (`(:force-redirect-append ,pn)
             (c `(>>! 1 ,pn))
             (c `(>& 2 1)))

           (_
            (flush-argument c)
            (parse-command-spec-token c x)
            (flush-argument c)))))
    (etypecase x
      (null ())
      (cons (c x))
      (simple-command-line-token
       (add-argument c (token-string x))))))

(defun parse-command-spec-token (c x)
  (labels
      ((e (x) (extend-argument c x))
       (p (x)
         (etypecase x
           (null ())
	   (cons (c x))
           ((eql -) (e " "))
	   (simple-command-line-token
	    (e (token-string x)))))
       (c (x)
         (match x
           (`(+ ,@args) ;; recurse (explicit call)
             (map () #'p args))
           (`(* ,@args) ;; splice
             (loop :for (arg . rest) :on args :do
               (p arg) (when rest (flush-argument c))))
           (`(quote ,@args) ;; quote
             (e (escape-command
                 (parse-command-spec-tokens args))))
           (`(,(type simple-command-line-token) ,@_) ;; recurse
             (map () #'p x))
           (_
            (error "Unrecognized command-spec token ~S" x)))))
    (p x)))

(defun parse-command-spec-tokens (spec)
  (let ((c (make-instance 'command-parse)))
    (parse-command-spec-token c `(+ ,@spec))
    (command-arguments (command-parse-results c))))

(defmethod print-process-spec ((spec command-spec) &optional s)
  (with-slots (arguments redirections) spec
    (with-output (s)
      (escape-command arguments s)
      (when redirections
        (loop :for r :in redirections :do
          (princ " " s) (print-process-spec r s))))))

(defun print-process-sequence-joined (spec separator empty s &optional tail)
  (with-output (s)
    (let ((processes (sequence-processes spec)))
      (if processes
          (progn
            (princ "(" s)
            (loop :for (p . rest) :on processes :do
               (print-process-spec p s) (when rest (princ separator s)))
            (princ ")" s)
            (when tail (princ tail s)))
          (princ empty s)))))

(defmethod print-process-spec ((spec pipe-spec) &optional s)
  (print-process-sequence-joined spec " | " "cat" s))

(defmethod print-process-spec ((spec or-spec) &optional s)
  (print-process-sequence-joined spec " || " "false" s))

(defmethod print-process-spec ((spec and-spec) &optional s)
  (print-process-sequence-joined spec " && " "true" s))

(defmethod print-process-spec ((spec progn-spec) &optional s)
  (print-process-sequence-joined spec "; " "true" s))

(defmethod print-process-spec ((spec fork-spec) &optional s)
  (print-process-sequence-joined spec "; " "true" s " &"))

(defmethod print-process-spec ((spec string) &optional s)
  (output-string spec s))

(defmethod print-process-spec ((spec cons) &optional s)
  (print-process-spec (parse-process-spec spec) s))

(in-readtable :standard)

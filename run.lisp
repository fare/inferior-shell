#+xcvb (module (:depends-on ("macros" "process-spec")))

(in-package :inferior-shell)

(defvar *backend* :auto)

(defun on-host-spec (host spec)
  (if (current-host-name-p host)
      spec
      `(ssh ,host ,(print-process-spec spec))))

(deftype direct-command-spec ()
  '(and command-spec (satisfies direct-command-spec-p)))

(defun direct-command-spec-p (spec)
  (and (typep spec 'command-spec)
       (null (command-redirections spec))))

(defun run-spec (spec &rest keys &key &allow-other-keys)
  (let* ((command
          (if (consp spec)
            (parse-process-spec spec)
            spec))
         (command
          (etypecase command
            (direct-command-spec
             (command-arguments spec))
            (process-spec
             (print-process-spec spec))
            (string
             spec))))
    (apply 'run-program command keys)))

(defun run-process-spec (spec &rest keys &key host backend &allow-other-keys)
  (etypecase host
    (null
     (etypecase spec
       (string
        (apply 'run-spec spec keys))
       (cons
        (apply 'run-process-spec (parse-process-spec spec) keys))
       (process-spec
        (ecase (or backend *backend*)
          #+(and sbcl sb-thread unix)
          ((:sbcl)
           (apply 'sbcl-run spec keys))
          ((:auto)
           (apply 'run-spec spec keys))))))
    (string
     (apply 'run-process-spec (on-host-spec host spec) :host nil keys))
    (function
     (apply 'run-process-spec (funcall host spec) :host nil keys))))

(defun run/nil (cmd &rest keys
                &key time show host (on-error t)
                &allow-other-keys)
  "run command CMD. Unless otherwise specified, discard the subprocess's output and error-output."
  (when (eq on-error t) (setf on-error 'error))
  (when show
    (format *trace-output* "; ~A~%" (print-process-spec cmd)))
  (flet ((run-it ()
           (handler-bind
               ((subprocess-error #'(lambda (c)
                                      (if on-error (return-from run-it (call-function on-error c))
                                          (continue c)))))
             (apply 'run-process-spec cmd :ignore-error-status nil :host host keys))))
    (if time (time (run-it)) (run-it))))

(defun run (cmd &rest keys
            &key on-error time show host (output t op) (error-output t eop) &allow-other-keys)
  "run command CMD. Unless otherwise specified, copy the subprocess's output to *standard-output*
  and its error-output to *error-output*."
  (declare (ignore on-error time show host))
  (apply 'run/nil cmd `(,@(unless op `(:output ,output))
                        ,@(unless eop `(:error-output ,error-output))
                        ,@keys)))

(defun run/s (cmd &rest keys &key on-error time show host)
  "run command CMD, return its standard output results as a string.
  Unless otherwise specified, discard its error-output."
  (declare (ignore on-error time show host))
  (apply 'run/nil cmd :output 'string keys))

(defun run/ss (cmd &rest keys &key on-error time show host)
  "run command CMD, return its standard output results as a string like run/s,
  but strips the line ending off the result string very much like `cmd` or $(cmd) at the shell"
  (declare (ignore on-error time show host))
  (apply 'run/nil cmd :output :string/stripped keys))

(defun run/interactive (cmd &rest keys &key on-error time show host)
  "run command CMD interactively, connecting the subprocess's input, output and error-output
  to the same file descriptors as the current process"
  (declare (ignore on-error time show host))
  (apply 'run/nil cmd :input :interactive :output :interactive :error-output :interactive keys))


(defun slurp-stream-string/stripped (input-stream)
  (stripln (slurp-stream-string input-stream)))

(defmethod slurp-input-stream ((x (eql :string/stripped)) input-stream
                               &key &allow-other-keys)
  (slurp-stream-string/stripped input-stream))

(defun run/lines (cmd &rest keys &key on-error time show host)
  "run command CMD, return its standard output results as a list of strings, one per line,
discarding line terminators. Unless otherwise specified, discard error-output."
  (declare (ignore on-error time show host))
  (apply 'run/nil cmd :output :lines keys))

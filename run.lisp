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
  "run command CMD. Unless otherwise specified, discard the subprocess's output and error-output.
  See the documentation for INFERIOR-SHELL:RUN for other keyword arguments."
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
and its error-output to *error-output*. Return values for the output, error-output and exit-code.

The command specified by COMMAND can be a list as parsed by PARSE-PROCESS-SPEC, or a string
to execute with a shell (/bin/sh on Unix, CMD.EXE on Windows).

Signal a continuable SUBPROCESS-ERROR if the process wasn't successful (exit-code 0),
unless ON-ERROR is specified, which if NIL means ignore the error (and return the three values).

If OUTPUT is a pathname, a string designating a pathname, or NIL designating the null device,
the file at that path is used as output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*,
and under SLIME will be on your *inferior-lisp* buffer.
If it's T, output goes to your current *STANDARD-OUTPUT* stream.
Otherwise, OUTPUT should be a value that is a suitable first argument to
SLURP-INPUT-STREAM (qv.), or a list of such a value and keyword arguments.
In this case, RUN-PROGRAM will create a temporary stream for the program output;
the program output, in that stream, will be processed by a call to SLURP-INPUT-STREAM,
using OUTPUT as the first argument (or the first element of OUTPUT, and the rest as keywords).
The primary value resulting from that call (or NIL if no call was needed)
will be the first value returned by RUN-PROGRAM.
E.g., using :OUTPUT :STRING will have it return the entire output stream as a string.
And using :OUTPUT '(:STRING :STRIPPED T) will have it return the same string
stripped of any ending newline.

ERROR-OUTPUT is similar to OUTPUT, except that the resulting value is returned
as the second value of RUN-PROGRAM. T designates the *ERROR-OUTPUT*.
Also :OUTPUT means redirecting the error output to the output stream,
in which case NIL is returned.

INPUT is similar to OUTPUT, except that VOMIT-OUTPUT-STREAM is used,
no value is returned, and T designates the *STANDARD-INPUT*.

Use ELEMENT-TYPE and EXTERNAL-FORMAT are passed on
to your Lisp implementation, when applicable, for creation of the output stream.

One and only one of the stream slurping or vomiting may or may not happen
in parallel with the subprocess, depending on options and implementation,
and with priority being given to output processing.
Other streams are completely produced or consumed
before or after the subprocess is spawned, using temporary files.

RUN returns 3 values:
0- the result of the OUTPUT slurping if any, or NIL
1- the result of the ERROR-OUTPUT slurping if any, or NIL
2- either 0 if the subprocess exited with success status,
or an indication of failure via the EXIT-CODE of the process"
  (declare (ignore on-error time show host output error-output))
  (apply 'run/nil cmd `(,@(unless op `(:output t))
                        ,@(unless eop `(:error-output t))
                        ,@keys)))

(defun run/s (cmd &rest keys &key on-error time show host)
  "run command CMD, return its standard output results as a string.
  Unless otherwise specified, discard its error-output.
  See the documentation for INFERIOR-SHELL:RUN for other keyword arguments."
  (declare (ignore on-error time show host))
  (apply 'run/nil cmd :output 'string keys))

(defun run/ss (cmd &rest keys &key on-error time show host)
  "run command CMD, return its standard output results as a string like run/s,
  but strips the line ending off the result string very much like `cmd` or $(cmd) at the shell
  See the documentation for INFERIOR-SHELL:RUN for other keyword arguments."
  (declare (ignore on-error time show host))
  (apply 'run/nil cmd :output '(:string :stripped t) keys))

(defun run/interactive (cmd &rest keys &key on-error time show host)
  "run command CMD interactively, connecting the subprocess's input, output and error-output
  to the same file descriptors as the current process
  See the documentation for INFERIOR-SHELL:RUN for other keyword arguments."
  (declare (ignore on-error time show host))
  (apply 'run/nil cmd :input :interactive :output :interactive :error-output :interactive keys))

(defun run/i (cmd &rest keys)
  "alias for run/interactive"
  (apply 'run/interactive cmd keys))

(defun run/lines (cmd &rest keys &key on-error time show host)
  "run command CMD, return its standard output results as a list of strings, one per line,
discarding line terminators. Unless otherwise specified, discard error-output.
  See the documentation for INFERIOR-SHELL:RUN for other keyword arguments."
  (declare (ignore on-error time show host))
  (apply 'run/nil cmd :output :lines keys))

INFERIOR-SHELL: spawning shell commands and pipes from Common Lisp
==================================================================

What is inferior-shell?
-----------------------

This CL library allows you to spawn local or remote processes and shell pipes.
It lets me use CL in many cases where I would previously write shell scripts.
The name is a pun, in that this library can both
let you spawn inferior (children processes) shells, and
serve itself as an inferior (not so featureful) shell.
Because so many features of a shell are missing,
inferior-shell only brings down the low-hanging fruits of shell scripting;
yet CL is such a better programming language than the shell
(or other "scripting" languages) that it is already a great pleasure
to be able to write things in CL rather than in these languages.
More features will come, and/or you can use other CL libraries as a complement.

Inferior-shell recognizes a small domain-specific language to describe
commands or pipelines of commands, and some functions to actually run
these pipelines either locally or remotely (via ssh).
It will implicitly invoke ssh when asked to run a command on a remote host;
for best results, be sure to have passphrase-less access to these hosts
via e.g. ssh-agent.

The name inferior-shell was suggested by Michael Livshin,
as inspired by the equivalent notion in GNU Emacs.

Example use of inferior-shell, from the rpm system:

    (defun rpms-installed (&key (packagenames t) host)
      (run/lines
       `(pipe (rpm -qa)
              ,@(unless (eq packagenames t)
                  `((egrep ("^(" ,@(loop :for (name . more) :on packagenames
                                     :collect name :when more :collect "|")
                                 ")-[^-]+(-[^-]+)?$")))))
       :host host))


Limitations
-----------

By default, inferior-shell uses uiop:run-program
as its universal execution backend, and has its limitations,
which are as follows.

First, inferior-shell at this point only supports
synchronous execution of sub-processes.
For asynchronous execution, please use IOlib or executor.
IOlib requires C compilation and linking, and may or may not support Windows.
executor only supports select implementations.
A future extension to inferior-shell may use IOlib as a backend.

Second, supported platforms at this time include:
ABCL, Allegro, CLISP, ClozureCL, CMUCL, ECL, LispWorks, RMCL, SBCL, SCL, XCL.
Platforms NOT (yet) supported include:
CormanLisp (untested), GCL (untested), Genera (unimplemented), MKCL (untested).
On supported platforms, inferior-shell works on both Unix and Windows.


Exported Functionality
----------------------

The inferior-shell library creates a package INFERIOR-SHELL,
that exports the following macros and functions:

* `PARSE-PROCESS-SPEC SPEC`

  parse an expression in the process-spec mini-language into
  objects specifying a pipeline of processes to be executed.
  See the PROCESS-SPEC mini-language below.


* `PRINT-PROCESS-SPEC SPEC &OPTIONAL OUTPUT`

  print a process specification to given OUTPUT
  into a portable form usable by a Unix shell.
  OUTPUT is as per FORMAT's stream output argument,
  defaults to NIL for returning the result as a string.
  SPEC can be a parsed PROCESS-SPEC object,
  a CONS to be parsed by PARSE-PROCESS-SPEC,
  or a string for a process-spec that has already been formatted.


* `*CURRENT-HOST-NAMES*`

  a variable, a list of strings, the aliases for the localhost.
  You may need to initialize it if the defaults don't suffice.


* `CURRENT-HOST-NAME-P X`

  a function, returns true if X is a string member of *CURRENT-HOST-NAMES*


* `INITIALIZE-CURRENT-HOST-NAMES`

  function that initializes the *CURRENT-HOST-NAMES*
  with "localhost" and the results from $(hostname -s) and $(hostname -f).


* `RUN CMD &KEY ON-ERROR TIME SHOW HOST OUTPUT`

  `RUN` will execute the given command `CMD`, which can be
  a `CONS` to be parsed by `PARSE-PROCESS-SPEC`,
  a `PROCESS-SPEC` object already parsed,
  or a string to be passed to a Unix shell.
  `ON-ERROR` specifies behavior in case the command doesn't successfully exit
  with exit code 0. `NIL` means continue and return the error code;
  `T` (default) means signal the error (same as `:ON-ERROR 'SIGNAL`);
  any other value is called as by `UIOP:CALL-FUNCTION` with the condition as argument,
  and if it returns, its return values are returned by `RUN`.
  `TIME` is a boolean which if true causes the execution to be timed as per TIME.
  `SHOW` is a boolean which if true causes a message to be sent
  to the `*TRACE-OUTPUT*` before execution.
  `HOST` is either `NIL` (execute on localhost) or a string specifying a host
  on which to run the command using `ssh` if
  it's not an alias for localhost as recognized by `CURRENT-HOST-NAME-P`
  (be sure to have passphraseless login using `ssh-agent`).
  The `INPUT`, `OUTPUT` and `ERROR-OUTPUT` arguments are as for `UIOP:RUN-PROGRAM`,
  except that they default to `NIL`, `T`, `T` respectively instead of `NIL`, `NIL`, `NIL`.
  In particular, `OUTPUT` is as per `UIOP:SLURP-OUTPUT-STREAM` one of
  `NIL` for no output (redirect to `/dev/null`),
  a stream for itself,
  `T` (default) for the current `*standard-output*`,
  `:INTERACTIVE` for inheriting the parent process's stdout,
  `:LINES` for returning one result per line,
  `:STRING` for returning the output as one big string,
  `:STRING/STRIPPED` is like `:STRING`
  but strips any line-ending at the end of the results,
  just like a shell's ``cmd`` or `$(cmd)` would, and
  more options are accepted and you can define your own, as per
  uiop's slurp-input-stream protocol.
  On Windows, `RUN` will not succeed for pipes, only for simple commands.
  On Unix, simple commands on localhost are executed directly, but
  remote commands and pipes are executed by spawning a shell.

* `RUN/NIL CMD &KEY ON-ERROR TIME SHOW HOST`

  `RUN/NIL` is a shorthand for `RUN` with `:INPUT` `:OUTPUT` `:ERROR-OUTPUT` bound to `NIL`.


* `RUN/S CMD &KEY ON-ERROR TIME SHOW HOST`

  `RUN/S` is a shorthand for `RUN` with `:OUTPUT` bound to `:STRING`,
  returning as a string what the inferior command sent to its standard output.

* `RUN/SS CMD &KEY ON-ERROR TIME SHOW HOST`

  `RUN/S` is a shorthand for `RUN :OUTPUT :STRING/STRIPPED`,
  just like a shell's ``cmd`` or `$(cmd)` would do.


* `RUN/INTERACTIVE CMD &KEY ON-ERROR TIME SHOW HOST`

  `RUN/INTERACTIVE` is a shorthand for `RUN` with `:INPUT` `:OUTPUT` `:ERROR-OUTPUT`
  all bound to `:INTERACTIVE`, so you may run commands that interact with users,
  inheritting the stdin, stdout and stderr of the current process.

* `RUN/I CMD &KEY KEYS`

  `RUN/I` is a shorthand alias for `RUN/INTERACTIVE`

* `RUN/LINES CMD &KEY ON-ERROR TIME SHOW HOST`

  `RUN/LINES` is a shorthand for `RUN :OUTPUT :LINES`,
  returning as a list of one string per line (stripped of line-ending)
  what the inferior command sent to its standard output.

* `*BACKEND*`

  a variable to choose between backends. Currently, only supported are
  `:AUTO` (the default, using uiop:run-program, and
  spawning a shell unless it's a simple process), and
  `:SBCL` (only available on `#+(and sbcl sb-thread unix)`,
  doesn't need a shell but has some limitations such as
  only supporting redirection of stdin, stdout, stderr),
  and is not recommended (and not actively supported at the moment).


The PROCESS-SPEC mini-language
------------------------------

This library offers a SEXP syntax to specify processes
and pipelines of processes in the manner of Unix shells,
including support for file descriptor redirection.
Process specifications can be printed,
to be executed by a local or remote shell,
or directly executed by your Lisp implementation,
depending on its capabilities and on the complexity of the pipeline.

SEXP mini-language

```
;; A process is a pipe or a command
process := pipe | or | and | progn | fork | command

;; A pipe is a list of processes, each of whose output is connected to the next one's input.
pipe := ( pipe process* )

;; OR is a list of processes which will be executed in sequence until one returns exit code 0.
or := ( or processes )

;; AND is a list of processes which will be executed in sequence until one does not return exit code 0.
and := ( and processes )

;; PROGN is a list of processes which will be executed sequentially.
progn := ( progn processes )

;; FORK is a list of processes which will be forked and executed in parallel.
fork := ( fork processes )

;; A command is a list of tokens and redirections.
;; Tokens specify the argv, redirections specify modifications of the inherited file descriptors.
command := ( [redirection|token|tokens]* )

;; A token is a string, to be used literally,
;; a keyword, to be downcased and prefixed with -- as in :foo ==> "--foo"
;; a symbol, to be downcased, or a list of tokens to be concatenated.
token := string | keyword | symbol | (token*)

;; A list starting with * is actually to be spliced in the token stream.
tokens := (\* [token|tokens]*)

;; Redirections mimic those redirections available to a shell, for instance zsh.
redirection := (
 ! fd pathname flags |   ;; open a file with given flags redirect to specified fd
 < fd? pathname | ;; open a file for input, redirect to specified fd (default: 0)
 [>|>>|<>|>!|>>!] fd? pathname | ;; open a file for (respectively) output, append, io, output clobbering, append clobbering, redirect to specified fd (default: 1)
 - fd | <& fd - | >& fd - | ;; close a fd
 <& - | >& - | ;; close fd 0, respectively fd 1.
 <& fd fd | >& fd fd | ;; redirect fds: the left one is the new number, the right one the old number.
 >& pn | >&! | ;; redirect both fd 1 and 2 to pathname (respectively, clobbering)
 >>& pn | >>&! ) ;; redirect both fd 1 and 2 to append to pathname (respectively, clobbering)
```

Note that these are all exported symbols from the INFERIOR-SHELL package,
except that a few of them are also inherited from COMMON-LISP: < > -
Therefore the other ones will only work if you either
use the INFERIOR-SHELL package or
use a package prefix INFERIOR-SHELL: where appropriate.

A SEXP in the minilanguage can be parsed with parse-process-spec,
into an object of class process-spec.
print-process-spec will print a process-spec object;
in this context, a string represents itself (assuming it's already a printed process spec),
and a cons is a specification in the minilanguage to be parsed with parse-process-spec first.


TO DO
-----

Better document it.

Have a complementary inferior-shell-watcher library that uses iolib to spawn
pipes locally, and watch the subprocesses as part of the iolib event loop.

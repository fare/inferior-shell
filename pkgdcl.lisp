(uiop/package:define-package :inferior-shell
  (:mix :fare-utils :uiop :alexandria)
  (:use :cl :trivia :named-readtables :fare-mop)
  (:export
   #:run #:run/nil #:run/s #:run/ss #:run/interactive #:run/i #:run/lines
   #:simple-command-line-token #:token-string
   #:process-spec #:command-spec #:pipe-spec
   #:or-spec #:and-spec #:progn-spec #:fork-spec
   #:print-process-spec #:parse-process-spec
   #:command-arguments #:command-redirections
   #:redirection #:file-redirection #:fd-redirection #:close-redirection
   #:! #:- #:< #:> #:<> #:>! #:>> #:>>! #:<& #:>& #:>&! #:>>&!
   #:>& #:>> #:>>& #:pipe #:or #:and #:progn #:fork
   #:zglobcmd #:zglob))

;;; -*- Lisp -*-
#-asdf3 (error "inferior-shell requires ASDF 3.0.3 or later")

(defsystem "inferior-shell"
  :version "2.0.3.3"
  :description "spawn local or remote processes and shell pipes"
  :author "Francois-Rene Rideau"
  :license "MIT"
  :depends-on ((:version "asdf" "3.0.3") ; input and error-output redirection
               #+sbcl (:require "sb-posix")
               "alexandria" "optima"
               "fare-utils" "fare-quasiquote-extras" "fare-mop")
  :around-compile "uiop:call-with-safe-io-syntax" ;; ensures that quasiquote syntax doesn't escape
  :components
  ((:file "pkgdcl")
   (:file "process-spec" :depends-on ("pkgdcl"))
   (:file "utilities" :depends-on ("pkgdcl"))
   (:file "macros" :depends-on ("pkgdcl"))
   (:file "host" :depends-on ("pkgdcl"))
   (:file "run" :depends-on ("process-spec" "macros"))
   (:file "run-generic" :depends-on ("process-spec" "macros"))
   (:file "run-sbcl" :depends-on ("process-spec" "macros" "run-generic")
          :if-feature (:and :sbcl :sb-thread :unix)))
  :in-order-to ((test-op (load-op "inferior-shell/test")))
  :perform (test-op (o s) ;; symbol-call will only work if loaded with ASDF3
              (symbol-call :inferior-shell-test :test-suite)))

(defsystem "inferior-shell/test"
  :depends-on ("inferior-shell" "hu.dwim.stefil")
  :description "testing inferior-shell"
  :components ((:file "test")))

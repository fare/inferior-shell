(in-package :cl)

(defpackage :inferior-shell-test
  (:use :cl :inferior-shell :fiveam))

(in-package :inferior-shell-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(def-suite inferior-shell-test
  :description "Suite of tests for inferior shell.")

(in-suite inferior-shell-test)

(def-test basics ()
  (is (equal "1 2 3"
             (run/ss '(echo (1) "2" (+ 3)))))
  (is (equal "1 2 3"
             (run/ss "echo 1    2        3")))
  (is (equal "Hello, World!"
             (run/ss `(pipe (echo (+ hel "lo,") world)
                       (tr "hw" "HW") (sed -e "s/$/!/")))))
  (is (equal "hello"
             (run/ss `(pipe (echo ,(format nil "hello~%world"))
                       (grep "h"))))))

(def-test tokens ()
  (is (equal "Hello"
             (run/ss `(echo "Hello"))))
  (is (equal "hello"
             (run/ss `(echo hello))))
  (is (equal "hello"
             (run/ss `(echo |Hello|))))
  (is (equal " hello "
             (run/ss `(echo | Hello |))))
  (is (equal "--hello"
             (run/ss `(echo :hello)))))

(def-test + ()
  (is (equal (format nil "Hello world!")
             (run/ss `(echo (+ "Hello " world!))))))

(def-test and ()
  (is (equal (format nil "hello~%world")
             (run/ss `(and (echo hello)
                           (echo world)
                           (return 1)
                           (echo good morning))
                     :on-error nil))))

(def-test or ()
  (is (equal "hello"
             (run/ss `(or (return 1)
                          (echo hello)
                          (echo world))))))

#+unix
(def-test fork ()
  (let ((fifo "/tmp/inferior-shell.fifo"))
    (run/ss `(rm ,fifo) :on-error nil)
    (run/ss `(mkfifo ,fifo))
    (run/ss `(fork (echo "hello" (> ,fifo))))
    (is (equal "hello"
               (run/ss `(cat ,fifo))))
    (run/ss `(rm ,fifo) :on-error nil)))

(def-test progn ()
  (is (equal (format nil "Hello world!~%good morning")
             (run/ss `(progn (echo (+ "Hello " world!))
                             (echo '|Good| morning))))))

(def-test redirection ()
  ;; TODO Test all other redirection methods.
  (uiop:with-temporary-file (:pathname file)
    (run/ss `(rm ,file) :on-error nil)
    (run/ss `(echo "1234567890" (>> ,file)))
    (is (equal "1234567890"
               (run/ss `(cat ,file))))
    (run/ss `(echo "abcdefghij" (> ,file)))
    (is (equal "abcdefghij"
               (run/ss `(cat ,file))))
    (run/ss `(rm ,file))))

(run! 'inferior-shell-test::inferior-shell-test)

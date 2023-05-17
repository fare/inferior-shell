(in-package :cl)

(defpackage :inferior-shell-test
  (:use :cl :inferior-shell :fiveam))

(in-package :inferior-shell-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(def-suite inferior-shell-test
  :description "Suite of tests for inferior shell.")

(def-test test-inferior-shell (:suite inferior-shell-test)
  (is (equal (run/ss '(echo (1) "2" (+ 3))) "1 2 3"))
  (is (equal (run/ss "echo 1    2        3") "1 2 3"))
  (is (equal (run/ss `(pipe (echo (+ hel "lo,") world)
                            (tr "hw" "HW") (sed -e "s/$/!/")))
             "Hello, World!")))

(fiveam:run! 'inferior-shell-test::test-inferior-shell)

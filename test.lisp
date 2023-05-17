(in-package :cl)

(defpackage :inferior-shell-test
  (:use :cl :inferior-shell :fiveam))

(in-package :inferior-shell-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defmacro with-temp-file (symbol &rest body)
  `(let ((,symbol (format nil "/tmp/inf-shell-file-~a" (random 100000))))
     (run/ss `(rm ,,symbol) :on-error nil)
     (unwind-protect
          (progn ,@body)
       (run/ss `(rm ,,symbol) :on-error nil))))

(def-suite inferior-shell-test
  :description "Suite of tests for inferior shell.")

(def-test basics (:suite inferior-shell-test)
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

(def-test tokens (:suite inferior-shell-test)
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

(def-test + (:suite inferior-shell-test)
  (is (equal (format nil "Hello world!")
             (run/ss `(echo (+ "Hello " world!))))))

(def-test and (:suite inferior-shell-test)
  (is (equal (format nil "hello~%world")
             (run/ss `(and (echo hello)
                           (echo world)
                           (return 1)
                           (echo good morning))
                     :on-error nil))))

(def-test or (:suite inferior-shell-test)
  (is (equal "hello"
             (run/ss `(or (return 1)
                          (echo hello)
                          (echo world))))))

(def-test fork (:suite inferior-shell-test)
  (with-temp-file file
    (run/ss `(echo (> ,file)))
    (run/ss `(fork (sleep "0.2") (echo a (> ,file))))
    (is (equal "" (run/ss `(cat ,file))))
    (sleep 0.5)
    (is (equal "a" (run/ss `(cat ,file))))))

(def-test progn (:suite inferior-shell-test)
  (is (equal (format nil "Hello world!~%good morning")
             (run/ss `(progn (echo (+ "Hello " world!))
                             (echo '|Good| morning))))))

(def-test redirection (:suite inferior-shell-test)
  ;; TODO Test all other redirection methods.
  (with-temp-file file
    (run/ss `(rm ,file) :on-error nil)
    (run/ss `(echo "1234567890" (>> ,file)))
    (is (equal "1234567890"
               (run/ss `(cat ,file))))
    (run/ss `(echo "abcdefghij" (> ,file)))
    (is (equal "abcdefghij"
               (run/ss `(cat ,file))))
    (run/ss `(rm ,file))))

(fiveam:run! 'inferior-shell-test::inferior-shell-test)

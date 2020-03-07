(in-package :serapeum.tests)

(def-suite types :in serapeum)
(in-suite types)

(test tuple
  (is (typep '() '(tuple)))
  (is (not (typep '() '(tuple null))))
  (is (typep '(1) '(tuple integer)))
  (is (not (typep '(1) '(tuple symbol))))
  (is (typep '(1 :x #\c) '(tuple integer symbol character))))

(test supertypep
  (is (supertypep 'rational 'integer))
  (is (supertypep 'integer 'integer)))

(test proper-subtype-p
  (is (proper-subtype-p 'integer 'rational))
  (is (not (proper-subtype-p 'integer 'integer))))

(test true
  (is (true t))
  (is (not (true nil)))
  ;; NB The read base doesn't matter; 0 (or 1) is always a digit char.
  (is (eql t (true (digit-char-p #\0)))))

(test assure-pattern
  (is
   (stringp
    (trivia:match ""
      ((assure string x)
       x))))
  (is
   (null
    (trivia:match ""
      ((assure list x)
       x)))))

(test (with-key-fn :compile-at :run-time)
  (finishes
    (locally (declare (optimize (space 3) (speed 0)))
      (let ((key #'reverse))
        (serapeum::with-key-fn (key)
          (key "xyz")))))
  (finishes
    (locally (declare (optimize (speed 3) (space 0)))
      (let ((key #'reverse))
        (serapeum::with-key-fn (key)
          (key "xyz"))))))

(defmacro expansion-time-constant? (form &environment env)
  (nth-value 1 (eval-if-constant form env)))

(test with-boolean
  (let ((x t))
    (with-boolean (x)
      (is-true (expansion-time-constant? x)))))

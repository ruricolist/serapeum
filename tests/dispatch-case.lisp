(in-package :serapeum.tests)

(def-suite dispatch-case :in serapeum)
(in-suite dispatch-case)

(defpackage :serapeum.tests.dispatch-case-example
  (:use :cl :alexandria :serapeum
    :local-time)
  (:shadow :time))
(in-package :serapeum.tests.dispatch-case-example)

(deftype universal-time ()
  '(integer 0 *))

(deftype time ()
  '(or universal-time timestamp))

(defun time=/nested (t1 t2)
  (etypecase-of time t1
    (universal-time
     (etypecase-of time t2
       (universal-time
        (= t1 t2))
       (timestamp
        (= t1 (timestamp-to-universal t2)))))
    (timestamp
     (etypecase-of time t2
       (universal-time
        (time=/nested t2 t1))
       (timestamp
        (timestamp= t1 t2))))))

(defgeneric time=/generic (t1 t2)
  (:method ((t1 integer) (t2 integer))
    (= t1 t2))
  (:method ((t1 timestamp) (t2 timestamp))
    (timestamp= t1 t2))
  (:method ((t1 integer) (t2 timestamp))
    (= t1 (timestamp-to-universal t2)))
  (:method ((t1 timestamp) (t2 integer))
    (time=/generic t2 t1)))

(defun time=/dc (t1 t2)
  (dispatch-case ((t1 time)
                  (t2 time))
    ((universal-time universal-time)
     (= t1 t2))
    ((timestamp timestamp)
     (timestamp= t1 t2))
    ((universal-time timestamp)
     (= t1 (timestamp-to-universal t2)))
    ((timestamp universal-time)
     (time=/dc t2 t1))))

(defun time=/dc/rec (x y)
  (dispatch-case ((x time)
                  (y time))
    ((time universal-time)
     (time=/dc/rec x (universal-to-timestamp y)))
    ((universal-time time)
     (time=/dc/rec (universal-to-timestamp x) y))
    ((timestamp timestamp)
     (timestamp= x y))))

(5am:test dispatch-case
  (let* ((now/ut (get-universal-time))
         (then/ut 0)
         (then/ts (universal-to-timestamp 0))
         (now/ts (universal-to-timestamp now/ut)))
    (map-permutations
     (lambda (perm)
       (5am:is
        (equal*
         (mapcar #'time=/nested perm (rest perm))
         (mapcar #'time=/generic perm (rest perm))
         (mapcar #'time=/dc perm (rest perm))
         (mapcar #'time=/dc/rec perm (rest perm)))))
     (list now/ut then/ut then/ts now/ts))))

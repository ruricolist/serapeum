(in-package :serapeum.tests)

(def-suite dispatch-case :in serapeum)
(in-suite dispatch-case)

(test dispatch-case-error
  (fbind ((fn
           (lambda (x y)
             (dispatch-case ((x integer) (y string))
               ((integer string) t)))))
    (is (fn 0 ""))
    (signals serapeum/dispatch-case::dispatch-case-error
      (fn "" 1))
    (finishes
      (princ
       (nth-value 1
         (ignore-errors
          (fn "" 1)))
       (make-broadcast-stream)))))

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

(5am:test dispatch-caseql
  (flet ((process (x y)
           (dispatch-caseql ((x (member :x :y))
                             (y (member :x :y)))
             ((:x :x) :xs)
             ((:y :y) :ys)
             ((:x :y) :x)
             ((:y :x) :y))))
    (5am:is (eql :xs (process :x :x)))
    (5am:is (eql :ys (process :y :y)))
    (5am:is (eql :x (process :x :y)))
    (5am:is (eql :y (process :y :x)))))

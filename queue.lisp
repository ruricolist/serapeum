(in-package #:serapeum)

;;; Norvig-style queues, but wrapped in objects so they don't overflow
;;; the printer, and with a more concise, Arc-inspired API.

(export '(queue queuep
          enq deq front
          qlen qlist qconc
          queue-empty-p
          clear-queue))

(defun make-queue-cons ()
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(declaim (inline make-queue))

(defstruct (queue (:constructor make-queue (&aux (cons (make-queue-cons))))
                  (:predicate queuep))
  "A structure wrapping a cons queue."
  (cons nil :type cons :read-only t))

(defmethod print-object ((queue queue) stream)
  (if (and *print-readably* *read-eval*)
      (progn
        (format stream "#.")
        (print-object `(queue ,@(qlist queue)) stream))
      (print-unreadable-object (queue stream :type t)
        (format stream "~a" (qlist queue)))))

(defmethod make-load-form ((queue queue) &optional env)
  (declare (ignore env))
  (values `(make-queue)
          `(qconc ',queue (list ,@(qlist queue)))))

(defun queue (&rest initial-contents)
  "Build a new queue with INITIAL-CONTENTS."
  (lret ((q (make-queue)))
    (dolist (x initial-contents)
      (enq x q))))

(defun clear-queue (queue)
  "Return QUEUE's contents and reset it."
  (prog1 (qlist queue)
    (let ((q (queue-cons queue)))
      (setf (cdr q) nil
            (car q) q))))

(define-compiler-macro queue (&whole decline &rest xs)
  "When there are no initial elements, use the bare constructor,
allowing the queue to be declared dynamic-extent."
  (if xs
      decline
      `(make-queue)))

(defun qlen (queue)
  "The number of items in QUEUE."
  (length (qlist queue)))

(-> qlist (queue) list)
(defun qlist (queue)
  "A list of the times in QUEUE."
  (cdr (queue-cons queue)))

(defun enq (item queue)
  "Insert ITEM at end of QUEUE."
  (check-type queue queue)
  (let ((q (queue-cons queue)))
    (setf (car q)
          (setf (cdr (car q))
                (cons item nil))))
  queue)

(defun deq (queue)
  "Remove item from the front of the QUEUE."
  ;; Bizarrely, the version in PAIP returns the queue, not the
  ;; item dequeued. This version from Waters & Norvig,
  ;; "Implementing Queues in Lisp."
  (check-type queue queue)
  (let ((q (queue-cons queue)))
    (let ((items (cdr q)))
      (unless (setf (cdr q) (cdr items))
        (setf (car q) q))
      (car items))))

(defun front (queue)
  "The first element in QUEUE."
  (first (qlist queue)))

(defun queue-empty-p (queue)
  "Is QUEUE empty?"
  (not (qlist queue)))

(defun qconc (queue list)
  "Destructively concatenate LIST onto the end of QUEUE.
Return the queue."
  (check-type queue queue)
  (when (null list)
    (return-from qconc queue))
  (let ((q (queue-cons queue)))
    (setf (car q)
          (last (setf (cdr (car q)) list))))
  queue)

(in-package #:serapeum)

;;; Norvig-style queues, but wrapped in objects so they don't overflow
;;; the printer, and with a more concise, Arc-inspired API.

(defun make-queue-cons ()
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(declaim (inline make-queue queuep))

(defstruct (queue (:constructor make-queue (&aux (cons (make-queue-cons)))))
  "Basic cons queues, with an implementation based on PAIP and the
original Norvig & Waters paper, an an API mostly borrowed from Arc.

About Arc. For the most part, Arc-style identifiers are pessimal,
neither quite opaque nor quite explicit, like riddles. But by using
abbreviated names, we avoid the danger of clashing with
special-purpose queue implementations.

Create a queue with `queue', like `list':

    (queue 1 2 3) => #<QUEUE (1 2 3)>

Get the items with `qlist':

    (qlist (queue 1 2 3)) => '(1 2 3)

Add items with `enq':

    (enq 3 (queue 1 2)) => #<QUEUE (1 2 3)>

Remove an item with `deq':

    (deq (queue 1 2 3)) => 3

To (destructively) add a list to the end of the queue, use `qconc':

    (qconc (queue 1 2 3) '(4 5 6)) => #<QUEUE (1 2 3 4 5 6)>

The rest of the API:

- `qlen' Like `(length (qlist ...))'
- `clear-queue' Clear the queue
- `front' Like to `(car (qlist ...))'
- `queue-empty-p' Test if the queue is empty

The idea is that *collecting* is something we do often enough to
justifying making *collectors* (queues) first-class."
  (cons nil :type cons :read-only t))

(declaim-freeze-type queue)

(defun queuep (x)
  "Is X a queue?"
  (queue-p x))

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
  (let ((q (make-queue)))
    (dolist (x initial-contents q)
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

(declaim (ftype (function (queue) list) qlist))
(defun qlist (queue)
  "A list of the items in QUEUE."
  (cdr (queue-cons queue)))

(defun enq (item queue)
  "Insert ITEM at the end of QUEUE."
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

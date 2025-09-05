(in-package #:serapeum)

(defun make-queue-cons ()
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(declaim (inline make-queue queuep))

(defstruct (queue (:constructor make-queue (&aux (cons (make-queue-cons))))
                  (:copier nil)
                  (:predicate queuep))
  "Basic cons queues, with an implementation based on PAIP and the
original Norvig & Waters paper, and an API mostly borrowed from Arc.

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

    (deq (queue 1 2 3)) => 1

Prepend an item with `undeq':

    (qlist (undeq 1 (queue 2 3))) => '(1 2 3)

To (destructively) join a list to the end of the queue, use `qconc':

    (qconc (queue 1 2 3) '(4 5 6)) => #<QUEUE (1 2 3 4 5 6)>

The rest of the API:

- `queuep' Test for a queue
- `qlen' Like `(length (qlist ...))'
- `clear-queue' Clear the queue
- `front' Like `(car (qlist ...))'
- `qback' Get the last element of the queue
- `queue-empty-p' Test if the queue is empty
- `qappend' Non-destructively join a list to the end of the queue
- `qconc' Destructively join a list to the end of the queue
- `qprepend' Non-destructively join a list to the front of the queue

Note that support for both `deq' and `undeq' means that a queue is
also effectively a stack. (But not quite a double-ended queue: you can
push to either end, and pop from the front, but you can't pop from the
end.)

The idea is that *collecting* is something we do often enough to
justifying making *collectors* (queues) first-class."
  (cons (error "No cons!") :type cons :read-only t))

(declaim-freeze-type queue)

(setf (documentation 'queuep 'function)
      "Test for a queue.")

(defmethod print-object ((queue queue) stream)
  (if (and *print-readably* *read-eval*)
      (progn
        (format stream "#.")
        (print-object `(queue ,@(qlist queue)) stream))
      (print-unreadable-object (queue stream :type t)
        (format stream "~a" (qlist queue))))
  queue)

(defmethod make-load-form ((queue queue) &optional env)
  (declare (ignore env))
  (values `(make-queue)
          `(qconc ',queue (list ,@(qlist queue)))))

(-> queue (&rest t) (values queue &optional))
(defun queue (&rest initial-contents)
  "Build a new queue with INITIAL-CONTENTS."
  (qappend (make-queue) initial-contents))

(define-compiler-macro queue (&whole decline &rest xs)
  "When there are no initial elements, use the bare constructor,
allowing the queue to be declared dynamic-extent."
  (if xs
      decline
      `(make-queue)))

(defpattern queue (&rest args)
  (with-unique-names (it)
    `(trivia:guard1 ,it
                    (typep ,it 'queue)
                    (qlist ,it)
                    (list ,@args))))

(-> clear-queue (queue) list)
(defun clear-queue (queue)
  "Return QUEUE's contents and reset it."
  (prog1 (qlist queue)
    (let ((q (queue-cons queue)))
      (setf (cdr q) nil
            (car q) q))))

(-> qlen (queue) array-length)
(defun qlen (queue)
  "The number of items in QUEUE."
  (length (qlist queue)))

(-> qlist (queue) list)
(defun qlist (queue)
  "A list of the items in QUEUE.
Does not cons."
  (cdr (queue-cons queue)))

(-> enq (t queue) queue)
(defun enq (item queue)
  "Insert ITEM at the end of QUEUE."
  (let ((q (queue-cons queue)))
    (setf (car q)
          (setf (cdr (car q))
                (cons item nil))))
  queue)

(-> deq (queue) t)
(defun deq (queue)
  "Remove item from the front of the QUEUE."
  ;; Bizarrely, the version in PAIP returns the queue, not the
  ;; item dequeued. This version from Waters & Norvig,
  ;; "Implementing Queues in Lisp."
  (let* ((q (queue-cons queue))
         (items (cdr q)))
    (unless (setf (cdr q) (cdr items))
      (setf (car q) q))
    (car items)))

(-> undeq (t queue) queue)
(defun undeq (item queue)
  "Add an item to the front of QUEUE.
For an empty queue, this does the same thing as ENQ.

For a queue with elements, this adds a new element onto the front of
queue (like pushing to an ordinary list).

This is called `undeq' because it can be used to undo a `deq'."
  (let ((q (queue-cons queue)))
    (if (cdr q)
        (push item (cdr q))
        (enq item queue))
    queue))

(-> queue-empty-p (queue) boolean)
(defun queue-empty-p (queue)
  "Is QUEUE empty?"
  (not (qlist queue)))

(-> front (queue) t)
(defun front (queue)
  "The first element in QUEUE."
  (first (qlist queue)))

(defun (setf front) (value queue)
  (if (queue-empty-p queue)
      (enq value queue)
      (setf (car (qlist queue))
            value))
  value)

(-> qback (queue) (values t &optional))
(defun qback (queue)
  "Get the last element of a queue."
  (unless (queue-empty-p queue)
    (caar (queue-cons queue))))

(defun (setf qback) (value queue)
  (if (queue-empty-p queue)
      (enq value queue)
      (setf (caar (queue-cons queue))
            value))
  value)

(-> qconc (queue list) queue)
(defun qconc (queue list)
  "Destructively concatenate LIST onto the end of QUEUE.
Return the queue."
  (when (null list)
    (return-from qconc queue))
  (let ((q (queue-cons queue)))
    (setf (car q)
          (last (setf (cdr (car q)) list))))
  queue)

(-> qappend (queue list) queue)
(defun qappend (queue list)
  "Append the elements of LIST onto the end of QUEUE.
Return the queue."
  (if list
      ;; It's probably faster to just copy LIST once and concatenate
      ;; it than to access the queue for each element.
      (qconc queue (copy-list list))
      queue))

(defun qprepend (list queue)
  "Insert ITEMS at the beginning of QUEUE."
  (qpreconc (copy-list list) queue))

(defun qpreconc (list queue)
  "Destructively splice LIST at the beginning of QUEUE."
  (let ((q (queue-cons queue)))
    (if (cdr q)
        (setf (cdr q)
              (nconc list (cdr q)))
        (qconc queue list))
    queue))

(-> copy-queue (queue) (values queue &optional))
(defun copy-queue (queue)
  "Copy QUEUE as another queue."
  (qappend (queue) (qlist queue)))

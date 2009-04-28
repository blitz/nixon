;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2009, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

(defun bench-tries (d)
  (declare (optimize (speed 3) (safety 0)))
  (let ((old-counter (cs-word d :counter))
        (tries 0))
    (declare (type fixnum tries))
    (target-nmi (target-of d))
    (loop 
       (if (= old-counter (cs-word d :counter))
           (incf tries)
           (return)))
    tries))


(defun bench-interrupt (d)
  (declare (optimize (speed 3) (safety 0)))
  (let ((old-counter (cs-word d :counter)))
    (target-nmi (target-of d))
    (loop 
       (unless (= old-counter (cs-word d :counter))
         (return)))))


(defun benchmark (fn &key (count 1000))
  (declare (optimize (speed 3))
           (type function fn)
           (fixnum count))
  (let ((start (get-internal-real-time)))
    (loop 
       repeat count
       do (funcall fn))
    (/ (coerce (- (get-internal-real-time)
                  start)
               'double-float)
       (* count internal-time-units-per-second))))

(defun bench-handler-cycles (d)
  (declare (optimize (speed 3) (safety 0)))
  (let ((old-counter (cs-word d :counter)))
    (target-nmi (target-of d))
    (loop 
       (unless (= old-counter (cs-word d :counter))
         (return)))
    (sleep 0.01)
    (cs-word d :eip)))

;;; Pentium 4:
;;; warm:   ~616 cycles in handler (+ two task switches)
;;; worst:  ~988 cycles in handler (+ two task switches)
;;;       0.3ms on host per interrupt
;;; -> 2700 samples per second
;;; -> ~60% overhead?

;;; EOF

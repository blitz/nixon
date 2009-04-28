;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2009, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)


(defun profile-interrupt (d)
  (let ((old-counter (cs-word d :counter)))
    (target-nmi (target-of d))
    (loop 
       (unless (= old-counter (cs-word d :counter))
         (return)))))

(defun profile-run (d tries)
  ;; Set "do nothing" in the command space and send it to the target.
  (setf (cs-command (command-space-of d)) 0)
  (store-cs d)
  ;; Clear our cache. This is mainly a precaution if you want to
  ;; access memory after profiling.
  (cache-clear (va-cache-of d))
  (let ((profile-hash (make-hash-table :size 1037)))
    (unwind-protect
         (iter (repeat tries)
               (profile-interrupt d)
               (incf (gethash (cs-word d :eip) profile-hash 0))
               #+ ignore (sleep 0.01)
               )
      (return-from profile-run
        profile-hash))))

;;; EOF

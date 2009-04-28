;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

;;; Nova implementation

(defun scan-regions (regions step fn)
  (loop 
     for (start length) in regions
     do (loop for pos upfrom start below (+ start length) by step
             do (funcall fn pos))))


(defmethod command-space-address ((d controller))
  (or (slot-value d 'command-space-address)

      (setf (slot-value d 'command-space-address)
            (progn
              (warn "No command space address supplied. Do that next time!")
              (scan-for-command-space (target-of d))))))

(defun scan-for-command-space (target &key (start 0) (step 4096)) 
  ;; XXX Scan the first 6MB of memory disregarding all
  ;; magic memory regions...
  (format *trace-output* "Scanning for command space. This may take a while... ")
  (let ((try 0))
    (scan-regions `((,start ,(* 10 1024 1024))) step
                  (lambda (addr)
                    ;; XXX print address ? ...
                    (format *trace-output* "~C~C"
                            (char " .oOo." (rem (incf try) 6))
                            #\Backspace)
                    (force-output *trace-output*)
                    (when (equalp +nmi-magic-value+
                                  (vector-to-uint32 (target-read target addr 4)))
                      (format *trace-output* "~C Found at ~X.~%" #\Backspace addr)
                      (return-from scan-for-command-space
                        addr)))))
  (error "Command Space not found."))

;;; Linux test stuff

#+ ignore
(progn
 (defvar *s*)
 (defvar *c*)

 (multiple-value-setq (*s* *c*) 
   (nova-server :target (create-target "qemu" "")
                )))

;;; EOF

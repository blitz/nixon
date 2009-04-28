;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

;;; Interface to the outside world

(defvar *target-names* '(("firewire" . firewire-target)
                         ("qemu" . qemu-target)))

(defun create-target (name &optional (connect-string ""))
  (let ((class (rest (assoc name  *target-names* :test #'string-equal))))
    (make-instance class :connect connect-string)))

;;; Abstract interface

(defcondition* target-not-found (error)
  ())

(defclass* target ()
  ())

(defgeneric target-max-request (target))
(defgeneric target-read  (target address size))
(defgeneric target-write (target address buffer))
(defgeneric target-nmi   (target))
(defgeneric target-interrupt (target message)
  (:documentation "Interrupt the target using an MSI message."))

(defgeneric target-destroy (target)
  (:method ((target target))
    (values)))

;;; EOF

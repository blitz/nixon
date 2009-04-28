;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2009, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

(defconstant +nop+ #x90)
(defconstant +call-far-absolute+ #x9A)
(defconstant +call-near-relative+ #xE8)
(defconstant +nova-kernel-cs+ #x0008)

;;; XXX Have to be changed when Nova is recompiled.
(defparameter *payload-target-va* #xc0886000) ; cheatspace (va)
(defparameter *payload-target-pa* (+ #x00886000
                                     #x00400000)) ; cheatspace (pa)
(defparameter *inject-target-va*  #xc0001d54) ; gsi_vector (va)
(defparameter *inject-target-pa*  #x00401d54) ; gsi_vector (pa)

(defun load-payload (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let ((array (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence array in)
      array)))

(defun encode-call (location target)
  (let* ((base (+ location 8))
         (imm (- target base)))
    (vector +nop+ +nop+ +nop+
            +call-near-relative+
            (ldb (byte 8 0)  imm)
            (ldb (byte 8 8)  imm)
            (ldb (byte 8 16) imm)
            (ldb (byte 8 24) imm))))

(defun blockwise-write (target addr data)
  (cond
    ((> (length data) 512)
     (target-write target addr (subseq data 0 512))
     (blockwise-write target (+ addr 512) (subseq data 512)))
    (t
     (target-write target addr data))))

(defun inject-payload (target payload)
  (let ((*print-base* 16)
        (inject-data (target-read target *inject-target-pa* 8))
        (call-inst (let ((call-target (+ *payload-target-va* 8)))
                     (format t "~&Call target is ~X.~%" call-target)
                     (encode-call *inject-target-va* call-target))))
    (format t "~&At injection point ~A. Fixing payload.~%" inject-data)
    (setf (subseq payload 0 8) inject-data)
    (format t "Writing payload at ~X (~A bytes).~%" *payload-target-pa* (length payload))
    (format t "~A~%" payload)
    (blockwise-write target *payload-target-pa* payload)
    (format t "Hijacking gsi_vector at ~X.~%" *inject-target-pa*)
    (target-write target *inject-target-pa* call-inst)
    ))

(defun attach-to-hijacked-system (target)
  (handler-case
      (let ((controller
             (make-instance 'controller
                            :target target
                            :command-space-address 
                            (scan-for-command-space target
                                                    :start *payload-target-pa*
                                                    :step 16))))
        controller)
    (corrupt-command-space ()
      (format t "Seems not to be hijacked yet.~%")
      nil)))

#|
(defvar *t*)
(defvar *c*)
(inject-payload (setq *t* (create-target "qemu")) (load-payload "src/diplom/code/src/nova-payload/nova-payload.bin"))
(setq *c* (attach-to-hijacked-system *t*))
|#

;;; EOF

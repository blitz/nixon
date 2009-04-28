;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

;;; Firewire implementation

(defclass* firewire-target (target)
  (node))

(defmethod initialize-instance :after ((target firewire-target) &key (connect "")
                                       &allow-other-keys)
  (let* ((splitted (cl-ppcre:split "," connect))
         (guid-str (first splitted))
         (port-n (parse-integer (or (second splitted) "0")))
         (port (make-instance 'raw1394-port :port port-n)))
    (setf (node-of target) 
          (if guid-str 
              (or (raw1394-find-node port (let ((guid (raw1394-guid-from-string guid-str)))
                                            (lambda (port n)
                                              (declare (ignore port))
                                              (raw1394-guid-equal guid (raw1394-node-guid n)))))
                  (error 'target-not-found))
              (raw1394-node port 0)))))

(defmethod target-max-request ((target firewire-target))
  (raw1394-max-request (node-of target)))

(defmethod target-read ((target firewire-target) address size)
  (raw1394-read (node-of target) address size))

(defmethod target-write ((target firewire-target) address buffer)
  (raw1394-write (node-of target) address buffer))

(defmethod target-nmi ((target firewire-target))
  (raw1394-write (node-of target) #xFEE00000 #(00 04 00 00)))

(defmethod target-interrupt ((target firewire-target) message)
  ;; XXX Default to little endian.
  (raw1394-write (node-of target) #xFEE00000
                 (let ((*endian* :little-endian))
                   (with-binary-output-to-vector (out 4)
                     (write-binary 'uint32 out message)))))

;;; EOF

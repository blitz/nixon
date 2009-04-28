;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

(defclass* qemu-target (target)
  (client))

(defmethod initialize-instance :after ((target qemu-target) &key (connect "")
                                       &allow-other-keys)
  (let* ((splitted (cl-ppcre:split "," connect))
         (host (or (first splitted) "localhost"))
         (port (parse-integer (or (second splitted)
                                  "1234")))
         (client (handler-case
                     (do-gdb-connection host port)
                   (usocket:connection-refused-error ()
                     (error 'target-not-found)))))
    (setf (client-of target) client)
    (qemu-sync client)
    (qemu-continue client)
    (qemu-flush client)))

(defun qemu-read-memory (client addr size)
  (send-raw-command client (format nil "m~X,~X"
                                   addr size))
  ;; Use a loop because the qemu stub seems to send a lot of Txx
  ;; responses quite arbitrary.
  (loop 
     for result = (get-result client)
     thereis (and (not (zerop (length result)))

                  (not (find (char result 0) "STOE"))  ; Not stop result or OK/Exx
                  (from-hex-string result))))

(defun qemu-flush (client)
  (loop for res = (get-result client nil)
       while res))

(defun qemu-sync (client)
  ;; Verify that the target stopped and eat all responses. This is a
  ;; bit hacky, but as long as it works...
  (qemu-flush client)
  (send-raw-command client "?")
  (get-result client)
  (qemu-flush client))

(defun qemu-continue (client)
  (qemu-flush client)
  (send-raw-command client "c"))

(defmethod target-max-request ((target qemu-target))
  ;; XXX Completely arbitrary...
  1024)

(defmethod target-read ((target qemu-target) address size)
  (qemu-sync (client-of target))
  (unwind-protect
       (qemu-read-memory (client-of target) address size)
    (qemu-continue (client-of target))))

(defmethod target-write ((target qemu-target) address buffer)
  (qemu-sync (client-of target))
  (unwind-protect
       (progn
         (send-raw-command (client-of target)
                           (format nil "M~X,~X:~A"
                                   address (length buffer)
                                   (to-hex-string buffer)))
         (get-result (client-of target)))
    (qemu-continue (client-of target))))

(defmethod target-nmi ((target qemu-target))
  (qemu-sync (client-of target))
  (unwind-protect
       (progn
         (send-raw-command (client-of target)
                         (format nil "qRcmd,~A"
                                 (to-hex-string 
                                  #+ sbcl (sb-ext:string-to-octets "nmi 0")
                                  #- sbcl (error "Nay"))))
         (get-result (client-of target)))
    (qemu-continue (client-of target))))

;;; EOF

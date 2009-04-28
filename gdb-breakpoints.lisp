;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

(defvar *poll-interval* (/ 20))         ; 20 times per second

(defconstant +max-hardware-breakpoints+ 4
  "Maximum amount of concurrently active hardware breakpoints. (x86)")

(defclass* hard-breakpoint ()
  (breakpoint-type address length slot))

(defclass* soft-breakpoint ()
  (address original))

(defun find-empty-slot (server)
  "Checks all set breakpoints to find a new empty slot."
  (loop 
     for slot from 0 below +max-hardware-breakpoints+
     thereis (and (not (find slot (hard-breakpoints-of server) :key #'slot-of))
                  slot)))

(defun update-breakpoints (server)
  "Update debug registers on the target"
  (let* ((c (controller-of server))
         (cs (command-space-of c))
         (bps (hard-breakpoints-of server)))
    (flet ((type-to-bits (type)
             (ecase type
               (:hardware #b00)
               ((:read :access) #b11)   ; doesn't break on instruction fetch.
               (:write #b01)))
           (length-to-bits (length)
             (ecase length
               (1 #b00)
               (2 #b01)
               (4 #b11))))
      ;; Reset status
      (setf (cs-dr6 cs) #b11111111111111100001111111110000
            (cs-dr7 cs) #x400)          ; All breakpoints and global detect disabled.
      ;; Breakpoint slot 0
      (macrolet ((db-setter (slot-no reg)
                   (let ((slot (gensym "SLOT")))
                     `(let* ((,slot ,slot-no)
                             (bp (find ,slot bps :key #'slot-of)))
                       (when bp
                         (setf (,reg cs) (address-of bp))
                         (setf (ldb (byte 2 (* 2 ,slot)) (cs-dr7 cs)) #b10 ; Global Enable
                               (ldb (byte 2 (+ 16 (* 4 ,slot))) (cs-dr7 cs)) (type-to-bits (breakpoint-type-of bp))
                               (ldb (byte 2 (+ 18 (* 4 ,slot))) (cs-dr7 cs)) 
                               (cond 
                                 ((eq (breakpoint-type-of bp) :hardware)
                                  (when (/= (length-of bp) 1)
                                    (warn "Invalid instruction breakpoint length. Forcing to 1."))
                                  (length-to-bits 1))
                                 (t (length-to-bits (length-of bp))))))))))
        (db-setter 0 cs-dr0)
        (db-setter 1 cs-dr1)
        (db-setter 2 cs-dr2)
        (db-setter 3 cs-dr3)))))

(defmethod gdb-insert-breakpoint ((server nixon-server) type address length)
  (cond
    ((eq type :software)
     ;; We ignore LENGTH for software breakpoints.
     (unless (find address (soft-breakpoints-of server) :key #'address-of)
       (let ((orig (read-va (controller-of server) address 1)))
         (push (make-instance 'soft-breakpoint
                              :address address
                              :original orig)
               (soft-breakpoints-of server))
         ;; Write INT3 at address.
         (write-va (controller-of server) address #(#xCC))))
     "OK")
    ((not (find length '(1 2 4))) "EInvalidLength")
    (t       
     (let ((slot (find-empty-slot server)))
       (cond
         (slot
          (let ((bp (make-instance 'hard-breakpoint 
                                   :slot slot
                                   :breakpoint-type type
                                   :address address
                                   :length length)))
            (format *trace-output* "~&New breakpoint in slot ~A.~%" slot)
            (push bp (hard-breakpoints-of server))
            (update-breakpoints server)
            "OK"))
         (t
          "ENoDebugRegistersLeft"))))))

(defun remove-soft-breakpoint (server bp)
  (assert (find bp (soft-breakpoints-of server)))
  ;; Restore original byte at breakpoint target
  (write-va (controller-of server) (address-of bp) (original-of bp))
  (setf (soft-breakpoints-of server) (remove bp (soft-breakpoints-of server)))
  (values))

(defun clear-software-breakpoints (server)
  (mapc (lambda (bp)
          (remove-soft-breakpoint server bp))
        (soft-breakpoints-of server)))

(defun clear-hardware-breakpoints (server)
  (setf (hard-breakpoints-of server) nil)
  (update-breakpoints server))

(defmethod gdb-remove-breakpoint ((server nixon-server) type address length)
  (declare (ignore length))
  (case type
    (:software 
     (let ((soft-bp (find address (soft-breakpoints-of server) :key #'address-of)))
       (when soft-bp
         (remove-soft-breakpoint server soft-bp))))
    (t
     (setf (hard-breakpoints-of server)
           (remove address (hard-breakpoints-of server) :key #'address-of))))
  (update-breakpoints server)
  "OK")

;;; Continuing from breaks

(defun return-code (reason &optional eip) 
  ;; 8 = EIP's register number
  ;; XXX
  (format nil "T~2,'0X" reason
          #+ ignore 
          (if eip
              (string-downcase (format nil "8:~8,'0X;" (sb-fasl::octet-swap eip 32)))
              "")))

(defmethod gdb-why-stop ((server nixon-server))
  (if (inside-debugger? (controller-of server))
      (return-code +reason-trap+)
      (return-code +reason-interrupt+)))


(defmethod gdb-continue-at ((server nixon-server) addr)
  (let ((c (controller-of server)))
    (when addr
      (setf (tss-eip (tss-of c)) addr))
    (resume server)
    (loop 
       ;; Poll for condition of remote target every now and then. Not
       ;; very pretty...
       (unless (zerop *poll-interval*)
         (sleep *poll-interval*))
       ;; Check to see if GDB wants a break or if the target has hit a
       ;; breakpoint.
       (let ((int? (check-interrupt server))
             (db? (inside-debugger? c)))
         (when (or int? db?)
           (stop server)
           ;; If we are interrupted or inside the debugger return to
           ;; GDB.
           (when db?
             ;; A debug condition (via debug extensions or INT3)
             ;; happened. If it was a INT3, decrement EIP.
             (let ((eip (tss-eip (tss-of c))))
               (when (find (1- eip) (soft-breakpoints-of server) :key #'address-of)
                 (decf (tss-eip (tss-of c))))))
           (when (or int? db?)
             (return-from gdb-continue-at
               (return-code
                (if db?
                    +reason-trap+
                    +reason-interrupt+)
                (tss-eip (tss-of c)))))
           ;; Otherwise continue the target.
           (resume c)
           )))))


(defun cleanup-target (server)
  (clear-software-breakpoints server)
  (clear-hardware-breakpoints server)
  (resume server))

(defmethod gdb-detach ((server nixon-server))
  (cleanup-target server)
  ;; Call next method to really connection.
  (call-next-method))

(defmethod gdb-kill ((server nixon-server))
  (format *trace-output* "Killing is not supported. Hit the reset button manually.~%")
  (cleanup-target server)
  ;; Call next method to really terminate the connection.
  (call-next-method))

(defmethod gdb-single-step-at ((server nixon-server) addr)
  (let ((c (controller-of server)))
    (setf (tss-eflags (tss-of c)) (logior +eflags-tp+ (tss-eflags (tss-of c))))
    (prog1
        ;; Sleeping only slows us down.
        (let ((*poll-interval* 0))
          (gdb-continue-at server addr))
      (setf (tss-eflags (tss-of c)) (logand (lognot +eflags-tp+) (tss-eflags (tss-of c)))))))

;;; EOF

;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

(defclass* nixon-server (target-x86 gdb-server)
  ((controller)
   (running? :initform t :accessor running?)
   (hard-breakpoints :initform '())
   (soft-breakpoints :initform '())))

;;; Simple memory access

(defmethod gdb-read-memory ((server nixon-server) addr size)
  (handler-case
      (read-va (controller-of server) addr size)
    (raw1394-error (c)
      (format *trace-output* "Error in GDB-READ-MEMORY: ~A~%" c)
      "E00")
    (no-mapping (c)
      (format *trace-output* "No mapping for ~X" (addr-of c))
      #())))

(defmethod gdb-write-memory ((server nixon-server) addr data)
  #+ ignore (format *trace-output* "Writing ~A@~X~%" data addr)
  (handler-case
      (progn
        (write-va (controller-of server) addr data)
        "OK")
    (raw1394-error (c)
      (format *trace-output* "Error in GDB-WRITE-MEMORY: ~A~%" c)
      "E00")
    (no-mapping (c)
      (format *trace-output* "No mapping for ~X" (addr-of c))
      "E00")))

;;; Register access

(defmethod gdb-read-registers ((server nixon-server))
  (let ((tss (tss-of (controller-of server))))
    (make-x86-register-set :eax (tss-eax tss)
                           :ecx (tss-ecx tss)
                           :edx (tss-edx tss)
                           :ebx (tss-ebx tss)
                           :esp (tss-esp tss)
                           :ebp (tss-ebp tss)
                           :esi (tss-esi tss)
                           :edi (tss-edi tss)
                           :eip (tss-eip tss)
                           :eflags (tss-eflags tss)
                           :cs (tss-cs tss)
                           :ss (tss-ss tss)
                           :ds (tss-ds tss)
                           :es (tss-es tss)
                           :fs (tss-fs tss)
                           :gs (tss-gs tss))))

(defmethod gdb-write-registers (server regs)
  (let ((tss (tss-of (controller-of server))))
    (setf (tss-eax tss) (reg-eax regs)
          (tss-ecx tss) (reg-ecx regs)
          (tss-edx tss) (reg-edx regs)
          (tss-ebx tss) (reg-ebx regs)
          (tss-esp tss) (reg-esp regs)
          (tss-ebp tss) (reg-ebp regs)
          (tss-esi tss) (reg-esi regs)
          (tss-edi tss) (reg-edi regs)
          (tss-eip tss) (reg-eip regs)
          (tss-eflags tss) (reg-eflags regs)
          (tss-cs tss) (reg-cs regs)
          (tss-ss tss) (reg-ss regs)
          (tss-ds tss) (reg-ds regs)
          (tss-es tss) (reg-es regs)
          (tss-fs tss) (reg-fs regs)
          (tss-gs tss) (reg-gs regs)))
  "OK")

(defmethod stop ((server nixon-server))
  (setf (running? server) nil)
  (interrupt (controller-of server) :stop t))

(defmethod resume ((server nixon-server))
  (setf (running? server) t)
  (store-tss (controller-of server))
  (interrupt (controller-of server) :store-dr t))

;;; This is called when GDB sents data on the command line and the
;;; target is still running.

(defmethod gdb-interrupt ((server nixon-server))
  (stop server))

;;; Threads

(defmethod gdb-set-thread ((server nixon-server) domain thread)
  (declare (ignore domain))
  (if (or (= thread -1)                 ; ALL
          (= thread 0))                 ; ANY
      "OK"
      "E00"))

;;; EOF

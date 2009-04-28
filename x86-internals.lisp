;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

;;; Globally set endianness
(setq *endian* :little-endian)

;;; Some useful constants
(defconstant +eflags-tp+ (ash 1 8))

;;; Task State Segment

(define-binary-struct TSS ()
  (previous-task 0 :binary-type u16)
  (reserved0     0 :binary-type u16)
  ;; Stack pointers
  (esp0          0 :binary-type u32)
  (ss0           0 :binary-type u16)
  (reserved1     0 :binary-type u16)
  (esp1          0 :binary-type u32)
  (ss1           0 :binary-type u16)
  (reserved2     0 :binary-type u16)
  (esp2          0 :binary-type u32)
  (ss2           0 :binary-type u16)
  (reserved3     0 :binary-type u16)
  
  ;; PDBR
  (cr3           0 :binary-type u32)

  ;; General purpose registers
  (eip           0 :binary-type u32)
  (eflags        0 :binary-type u32)
  (eax           0 :binary-type u32)
  (ecx           0 :binary-type u32)
  (edx           0 :binary-type u32)
  (ebx           0 :binary-type u32)
  (esp           0 :binary-type u32)
  (ebp           0 :binary-type u32)
  (esi           0 :binary-type u32)
  (edi           0 :binary-type u32)

  ;; Segment selectors
  (es            0 :binary-type u16)
  (reserved4     0 :binary-type u16)
  (cs            0 :binary-type u16)
  (reserved5     0 :binary-type u16)
  (ss            0 :binary-type u16)
  (reserved6     0 :binary-type u16)
  (ds            0 :binary-type u16)
  (reserved7     0 :binary-type u16)
  (fs            0 :binary-type u16)
  (reserved8     0 :binary-type u16)
  (gs            0 :binary-type u16)
  (reserved9     0 :binary-type u16)
  (ldt           0 :binary-type u16)
  (reserved10    0 :binary-type u16)

  (debug-trap    0 :binary-type u16)
  (iomapbase     0 :binary-type u16))

;;; Constants from nmi.h.
;;; XXX Grovel these constants.
(defconstant +nmi-magic-value+ #xC6482010)
(defconstant +nmi-verbose+     (ash 1 0))
(defconstant +nmi-dr-to-cs+    (ash 1 1))
(defconstant +nmi-cs-to-dr+    (ash 1 2))
(defconstant +nmi-stop+        (ash 1 3))

(defconstant +nmi-run+         (ash 1 0))
(defconstant +nmi-dbg+         (ash 1 1))
(defconstant +nmi-panic+       (ash 1 2))

(define-binary-struct (nmi-command-space
                       (:conc-name "CS-"))
    ()
  (magic         +nmi-magic-value+ :btt u32)
  
  ;; Written by the stub
  (inside-dbg    0 :btt u32)
  (counter       0 :btt u32)
  (status        0 :btt u32)
  (cr0           0 :btt u32)
  (cr3           0 :btt u32)
  (cr4           0 :btt u32)
  (tss-va        0 :btt u32)
  (eip           0 :btt u32)

  ;; Static fields.
  (description   "" :btt (define-null-terminated-string cs-description-string #.(* 16 4)))

  ;; Fields written by us.
  (command       0 :btt u32)
  (dr0           0 :btt u32)
  (dr1           0 :btt u32)
  (dr2           0 :btt u32)
  (dr3           0 :btt u32)
  (dr6           0 :btt u32)
  (dr7           0 :btt u32)

  (checksum      0 :btt u32))

;;; Some helper functions

(defun tss-from-vector (v)
  (with-binary-input-from-vector (s v)
    (read-binary 'tss s)))

(defun tss-to-vector (tss)
  (with-binary-output-to-vector (s)
    (write-binary 'tss s tss)))

(defun nmi-checksum (cs)
  ;; XXX Whooo. Binary types needs some improvement to handle cases
  ;; like these... Union support would be nice.
  (with-binary-input-from-vector 
      (in (nmi-command-space-to-vector cs :update-checksum nil))
    (iter (repeat (truncate (sizeof 'nmi-command-space) 4))
          (for val 
               first (read-binary 'u32 in)
               then (logand #xFFFFFFFF
                            (+ val
                               (read-binary 'u32 in))))
          (finally (return val)))))

(defcondition* corrupt-command-space (error)
  (command-space
   sum))

(defun nmi-command-space-from-vector (v &key (check t))
  ;; There is a special case where the checksum can be one. See dbg.S
  ;; in the Nova sources to find out why.
  (let ((cs (with-binary-input-from-vector (s v)
              (read-binary 'nmi-command-space s))))
    (let ((sum (nmi-checksum cs)))
      (when (and check (not (zerop sum)))
        (unless (and (= (cs-inside-dbg cs) 1)
                     (= sum 1))
          (error 'corrupt-command-space
                 :command-space cs
                 :sum sum))))
    cs))


(defun nmi-command-space-to-vector (cs &key (update-checksum t))
  (when update-checksum
    (setf (cs-checksum cs) 0)
    (setf (cs-checksum cs) (logand #xFFFFFFFF
                                   (1+ (lognot (nmi-checksum cs))))))
  (with-binary-output-to-vector (s)
    (write-binary 'nmi-command-space s cs)))

;;; Utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(ftype (function (t) (unsigned-byte 32)) vector-to-uint32)))

(defun vector-to-uint32 (vector)
  (assert (= 4 (length vector)))
  (values
   (with-binary-input-from-vector (in vector)
     (read-binary 'u32 in))))

(defun uint32-to-vector (uint32)
  (assert (typep uint32 '(unsigned-byte 32)))
  (with-binary-output-to-vector (out 4)
    (write-binary 'u32 out uint32)))

;;; EOF

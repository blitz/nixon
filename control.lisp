;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

;;; The controller is at the heart of Nixon. It controls (hence the
;;; name) execution of the target. Notable methods:
;;; 
;;; STOP controller
;;;  Stops execution on the target. Fetches the content of the command
;;;  space and TSS.
;;; 
;;; RESUME controller
;;;  Resumes execution.
;;;
;;; TSS-OF controller
;;;  Returns the TSS of the target.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(ftype (function (t t) (unsigned-byte 32)) cs-word)))

(defclass* controller ()
  ((target :initarg :target)
   (command-space-address :accessor nil :initarg :command-space-address
                          :initform nil)
   (command-space)                      ; set in initialize-instance :after
   (symbol-table :initform (make-hash-table :test 'equal))
   (va-cache :initform (make-instance 'memory-cache))
   (tss :initform nil)))

(defgeneric command-space-address (controller)
  (:documentation "Returns physical address of nmi_command_space"))

(defgeneric tss-address (controller)
  (:documentation "Returns physical address of the TSS"))

(defgeneric virt-to-phys (controller va))

(defgeneric inside-debugger? (controller))

;;; Access physical memory in the memory window defined by the stub.
(defgeneric write-phys (controller addr data))
(defgeneric read-phys (controller addr size))

(defgeneric write-va (controller addr data))
(defgeneric read-va (controller addr size))

(defgeneric interrupt (controller &key verbose retrieve-dr store-dr stop))

(defcondition* no-mapping (error)
  (addr))

;;; Initialization

(defmethod initialize-instance :after ((c controller) &rest args)
  "Fetch initial command space."
  (declare (ignore args))
  ;; Fetch and store CS
  (let ((cs (fetch-cs c)))
    ;; Print some info about the target
    (format t "~&The target:~%")
    (format t "~& claims to be '~A'~%" (cs-description cs))))

;;; Early interaction with the command space.

(defmethod cs-field ((d controller) field)
  "Returns the physical address a the CS field"
  (let ((phys-addr (+ (command-space-address d) 
                      (* 4 (or (loop 
                                  for cfield in '(:magic :inside-dbg :counter :status :cr0 :cr3 :cr4 :tss-va :eip)
                                  for pos upfrom 0
                                  thereis (and (eq cfield field)
                                               pos))
                               (error "No such command space field: ~A" field))))))
    phys-addr))

(defmethod cs-word ((d controller) field)
  (vector-to-uint32 (target-read (target-of d)
                                 (cs-field d field)
                                 4)))

(defmethod (setf cs-word) (new (d controller) field)
  (target-write (target-of d)
                (cs-field d field)
                (uint32-to-vector new))
  new)

;;; Retrieve target memory structures of interest.

(defmethod tss-address ((d controller))
  (virt-to-phys d (cs-tss-va (command-space-of d))))

(defmethod fetch-tss ((d controller))
  (setf (tss-of d) 
        (tss-from-vector (target-read (target-of d) 
                                      (tss-address d)
                                      (sizeof 'tss)))))

(defmethod store-tss ((d controller))
  (target-write (target-of d)
                (tss-address d)
                (tss-to-vector (tss-of d))))

(defmethod fetch-cs ((d controller))
  (setf (command-space-of d)
        (nmi-command-space-from-vector 
         (target-read (target-of d) (command-space-address d) (sizeof 'nmi-command-space)))))

(defmethod store-cs ((d controller))
  (let ((data (nmi-command-space-to-vector (command-space-of d) :update-checksum t)))
    (target-write (target-of d) (command-space-address d) data)))

;;; Low-level debugging functions

(defmethod inside-debugger? ((d controller))
  ;; We have to use CS-WORD instead of using our own copy of the
  ;; command space here, since the target may still be running and our
  ;; copy may not be accurate.  We have to reset this field ourselves.
  (not (zerop (cs-word d :inside-dbg))))

;;; Physical memory access

(defun in-range-p (addr size block-start block-size)
  (<= block-start
      addr 
      (+ addr (min 0 (1- size)))
      (+ block-start (1- block-size))))

(defmethod read-phys ((c controller) addr size)
  ;; This function used to implement memory windows.
  (target-read (target-of c) addr size))

(defmethod write-phys ((c controller) addr data)
  ;; This function used to implement memory windows.
  (target-write (target-of c) addr data))

;;; Virtual memory magic

(defconstant +cr4-pse+ (ash 1 4))
(defconstant +cr4-pae+ (ash 1 5))

(defmethod virt-to-phys ((d controller) va)
  (declare (type (unsigned-byte 32) va))
  (let* ((cr3 (cs-cr3 (command-space-of d)))
         (cr4 (cs-cr4 (command-space-of d)))
         (pse (logtest cr4 +cr4-pse+))
         (pae (logtest cr4 +cr4-pae+)))
    (when (zerop cr3)
      (error 'no-mapping :addr va))
    (cond
      ((and pse (not pae))
       ;; PSE Paging
       (let ((tab-entry (vector-to-uint32 (target-read (target-of d)
                                                       (+ cr3
                                                          (ash (ash va -22) 2))
                                                       4))))
         ;; Present?
         (unless (logtest tab-entry 1)
           (error 'no-mapping :addr va))
         ;; Test page size bit
         (if (logtest tab-entry (ash 1 7))
             ;; Large page
             (let ((hi (ldb (byte 4 13) tab-entry))
                   (lo (ldb (byte 10 22) tab-entry)))
               (logior
                (ash hi 32)
                (ash lo 22)
                (ldb (byte 22 0) va)))
             ;; Small page
             (let* ((ptab-addr (logand (lognot #xFFF) tab-entry))
                    (ptab-idx (ash (ldb (byte 10 12) va) 2))
                    (ptab-entry (vector-to-uint32 (target-read (target-of d)
                                                               (+ ptab-addr
                                                                  ptab-idx)
                                                               4))))
               (unless (logtest ptab-entry 1)
                 (error 'no-mapping :addr va))
               (logior (logand ptab-entry (lognot #xFFF))
                       (logand va #xFFF))))))
      (t
       (error "Paging scheme used by target not implemented.")))))

(defun map-page-wise (fn start-addr size max-request)
  "Call `fn' with address blocks in page size (but not exceeding
`max-request'). `fn' must take three parameters: POS, CURRENT-ADDRESS,
and SIZE. POS is the position in bytes from `start-addr'."
  (declare (type (unsigned-byte 32) start-addr size max-request)
           (optimize (debug 3)))
  ;; XXX Assumes 4K page sizes. Bigger pages are not a problem, so we
  ;; should be safe on x86.
  (labels ((rec (cur-addr remaining)
             (when (> remaining 0)
               (let* ((end-addr (min (logand #xFFFFFFFF
                                             (+ (logand cur-addr (lognot #xFFF)) #x1000))
                                     (+ cur-addr remaining)
                                     (+ cur-addr max-request)))
                      (size (- end-addr cur-addr)))
                 #+ ignore
                 (format *trace-output* "HUNK ~X ~X (~X)~%" cur-addr size (- cur-addr start-addr))
                 (funcall fn 
                          (- cur-addr start-addr) ; Position
                          cur-addr      ; current address
                          size)         ; size of block
                 (rec (+ cur-addr size)
                      (- remaining size))))))
    (rec start-addr size)
    (values)))

(defmethod read-va :around ((d controller) addr size)
  (let ((cache-data (query-cache (va-cache-of d) addr size)))
    (cond
      (cache-data cache-data)
      (t
       (let* ((block-size (block-size-of (va-cache-of d)))
              (aligned (cache-align (va-cache-of d) addr))
              ;; XXX Does this cause trouble if new-size is not a
              ;; multiple of block-size?
              (new-size (max (+ size (- addr aligned))
                             (* 2 block-size)))
              (data (call-next-method d aligned new-size)))
         (assert (<= aligned addr))
         (cache-insert (va-cache-of d) aligned data)
         (subseq data (- addr aligned) (+ size (- addr aligned))))))))


(defmethod read-va ((d controller) addr size)
  ;; Do not read across page boundaries and respect maximum read size.
  (let ((output (make-array size :element-type '(unsigned-byte 8))))
    (map-page-wise (lambda (pos addr size)
                     (let ((data (target-read (target-of d)
                                              (virt-to-phys d addr)
                                              size)))
                       (loop 
                          for byte across data
                          for i upfrom 0
                          do (setf (aref output (+ pos i)) byte))))
                   addr size (target-max-request (target-of d)))
    output))

(defmethod write-va :before ((d controller) addr data)
  (cache-invalidate (va-cache-of d) addr (length data)))

(defmethod write-va ((d controller) addr data)
  ;; Do not write across page boundaries and respect maximum write
  ;; size.
  (map-page-wise (lambda (pos addr size)
                   (target-write (target-of d)
                                 (virt-to-phys d addr) 
                                 (subseq data pos (+ pos size))))
                 addr (length data) 
                 (target-max-request (target-of d))))

;;; Top-level functions

(defmethod interrupt ((d controller) &key verbose retrieve-dr store-dr stop)
  (assert (not (and retrieve-dr store-dr)))
  (let* ((old-cs (command-space-of d))
         (counter (cs-counter old-cs)))
    (setf (cs-command old-cs) (logior (if verbose +nmi-verbose+ 0)
                                      (if retrieve-dr +nmi-dr-to-cs+ 0)
                                      (if store-dr +nmi-cs-to-dr+ 0)
                                      (if stop +nmi-stop+ 0))
          (cs-status old-cs) 0
          (cs-inside-dbg old-cs) 0)
    (store-cs d)
    (cache-clear (va-cache-of d))
    (target-nmi (target-of d))
    (loop
       ;(sleep 0.1)
       (unless (= counter (cs-word d :counter))
         (return))))
  ;; Get state
  (fetch-cs d)
  (fetch-tss d)
  (values))

;;; EOF

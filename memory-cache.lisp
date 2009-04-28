;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

;;; Meant for write-through operation
(defclass* memory-cache ()
  ((block-size-bits :initform 6)        ; 64 bytes
   (hash :initform (make-hash-table))
   (max-blocks :initform 102400)))

(defmethod print-object ((cache memory-cache) stream)
  (print-unreadable-object (cache stream :type t :identity t)
    (let ((count (hash-table-count (hash-of cache))))
    (format stream "~A entries (~A bytes)"
            count
            (* (block-size-of cache) count))))
  cache)

(defgeneric cache-align (cache address)
  (:method ((cache memory-cache) address)
    (logand address (lognot (1- (block-size-of cache))))))

(defgeneric block-size-of (cache)
  (:method ((cache memory-cache))
    (ash 1 (1- (block-size-bits-of cache)))))

(defgeneric cache-clear (cache)
  (:method ((cache memory-cache))
    (clrhash (hash-of cache))
    cache))

(defgeneric dump-cache (cache)
  (:method ((cache memory-cache))
    (format t "~&Dumping ~A.~%" cache)
    (maphash (lambda (address data)
               (format t "~&~8,'0X: ~A~%" address data))
             (hash-of cache))
    (format t "~&Done.~%")
    cache))

(defun do-block-wise (cache address size fn)
  ;; XXX There is a simpler function inside this heap of crap.
  (when (> size 0)
    (let* ((bs (block-size-of cache))
           (mask (lognot (1- bs)))
           (masked (logand mask address))
           (offset (- address masked)))
      (unless (zerop offset)
        (return-from do-block-wise
          (do-block-wise cache masked (+ size offset) fn)))
      (funcall fn masked)
      (do-block-wise cache (+ address bs) 
                     (max (- size bs) 0)
                     fn))))


(defgeneric query-cache (cache address size)
  (:method ((cache memory-cache) address size)
    (when (> size 0)
      (let ((output (make-array size :element-type '(unsigned-byte 8))))
        (do-block-wise cache address size
                       (lambda (cur-address)
                         (let ((res (gethash cur-address (hash-of cache))))
                           (unless res
                             (return-from query-cache
                               nil))
                           (if (< cur-address address)
                               (setf (subseq output 0) (subseq res (- address cur-address)))
                               (setf (subseq output (- cur-address address)) res)))))
        output))))



(defgeneric cache-insert (cache address data)
  (:method ((cache memory-cache) address data)
    (let ((bs (block-size-of cache))
          (size (length data)))
      (do-block-wise cache address size
                     (lambda (cur-addr)
                       #+ ignore (format *trace-output* "INSERT at ~X~%" cur-addr)
                       (when (and (>= cur-addr address)
                                  (<= (+ cur-addr bs) (+ address size)))
                         #+ ignore (format *trace-output* "INSERT at ~X: ok~%" cur-addr)
                         (setf (gethash cur-addr (hash-of cache))
                               (let ((start (- cur-addr address) ))
                                 (subseq data start (+ start bs))))))))
    cache))


(defgeneric cache-invalidate (cache address size)
  (:method ((cache memory-cache) address size)
    (do-block-wise cache address size
                   (lambda (curr-addr)
                     (remhash curr-addr (hash-of cache))))
    cache))


;;; EOF

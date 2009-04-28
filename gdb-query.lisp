;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

(defmethod gdb-monitor ((server nixon-server) monitor-cmd)
  (format t "~A~%" monitor-cmd)
  (regex-case monitor-cmd
    (("show breakpoints")
     (with-output-to-string (out)
       (format out "Software breakpoints:~%")
       (if (soft-breakpoints-of server)
           (loop for soft-bp in (soft-breakpoints-of server)
              do (format out "Address #x~8,'0X~%" (address-of soft-bp)))
           (format out "(none)~%"))
       (format out "~%Hardware breakpoints:~%")
       (if (hard-breakpoints-of server)
           (loop for hard-bp in (hard-breakpoints-of server)
              do (format out "Address #x~8,'0X Length ~A Slot ~A Type ~A~%"
                         (address-of hard-bp)
                         (length-of hard-bp)
                         (slot-of hard-bp)
                         (breakpoint-type-of hard-bp)))
           (format out "(none)~%"))))
    ;; XXX This should probably be disabled for a production version...
    (("reload")
     (asdf:oos 'asdf:load-op :nixon)
     (format nil "Reloaded successfully.~%"))
    (("clear-cache")
     (cache-clear (va-cache-of (controller-of server)))
     (format t "Cache cleared~%")
     "OK")
    (("version")
     (let ((system (asdf:find-system :nixon)))
       (format nil "~A ~A (on ~A ~A)~%" 
               (asdf:component-name system)
               (asdf:component-version system)
               (lisp-implementation-type)
               (lisp-implementation-version))))
    (t
     "")))

;;; The following is left as reminder how to do symbol lookup using
;;; GDB.
#+ ignore
(defmethod gdb-query ((server nixon-server) query-string)
  ;; XXX Symbol lookup is nice, but GDB asks us about stuff before we
  ;; have chance to do this. So this is not used for now.
  #|
  (flet ((emptyp (s)
           (zerop (length s))))
    (multiple-value-bind (match? submatches)
        (cl-ppcre:scan-to-strings "^Symbol:(.*):(.*)$" query-string)
      (when match?
        (let ((name (sb-ext:octets-to-string (from-hex-string (aref submatches 1))))
              (val (aref submatches 0)))
          (format *trace-output* "~A ~A~%" name val)
          ;; Check if we got a resolved symbol and store it.
          (when (not (emptyp name))
            (when (emptyp val)
              (error "GDB couldn't resolve ~A" name))
            (setf (gethash name (symbol-table-of (controller-of server)))
                  (parse-integer val :radix 16)))
          ;; Find a yet unresolved but interesting symbol.
          (loop 
             for sym in (interesting-symbols (controller-of server))
             do (unless (gethash sym (symbol-table-of (controller-of server)))
                  (return-from gdb-query
                    (format nil "qSymbol:~A" (to-hex-string (sb-ext:string-to-octets sym))))))
          ;; No symbol left -> return OK
          (return-from gdb-query "OK")))))
  |#
  ;; Unrecognized query.
  (format *trace-output* "Unknown query: ~A~%" query-string)
  "")

;;; EOF

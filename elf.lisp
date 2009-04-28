;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

;;; XXX This is not used at the moment and should be replaced by a
;;; proper ELF parser anyway if the need arises.

(defstruct elf-symbol
  num
  value
  size
  symtype
  bind
  visibility
  ndx
  name)

(defstruct elf-mapping
  va
  pa
  size)

(defmacro for-output-line ((line cmd &rest args) &body body)
  #- sbcl
  (error "Not implemented")
  #+ sbcl
  (let ((process (gensym "PROCESS")))
    `(let ((,process (sb-ext:run-program ,cmd (list ,@args)
                                         :search t
                                         :input nil
                                         :wait nil
                                         :output :stream)))
       (loop
          for ,line = (read-line (sb-ext:process-output ,process) nil nil)
          while ,line
          do (progn ,@body)))))


(defun extract-elf-symbols (binary)
  (let ((symbol-hash (make-hash-table :test 'equal)))
    (for-output-line (line "readelf" "-s" (namestring binary))
      (multiple-value-bind (match? m)
          (cl-ppcre:scan-to-strings "^ +([0-9]+): +([0-9a-fA-F]+) +([0-9]+) +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]+) +([^ ]*).*$" 
                                    line)
        (when match?
          (setf (gethash (aref m 7) symbol-hash)
                (make-elf-symbol :num (parse-integer (aref m 0))
                                 :value (parse-integer (aref m 1) :radix 16)
                                 :size (parse-integer (aref m 2))
                                 :symtype (aref m 3)
                                 :bind (aref m 4)
                                 :visibility (aref m 5)
                                 :ndx (aref m 6)
                                 :name (aref m 7))))))
    symbol-hash))


(defun extract-elf-mappings (binary)
  (let ((mappings ()))
    (for-output-line (line "readelf" "-l" (namestring binary))
      (multiple-value-bind (match? m)
          (cl-ppcre:scan-to-strings "^ +([^ ]+) +0x([^ ]+) +0x([^ ]+) +0x([^ ]+) +0x([^ ]+) +0x([^ ]+) +([RWE]+) +0x([^ ]+).*$"
                                    line)
        (when match?
          (push (make-elf-mapping :va (parse-integer (aref m 2) :radix 16)
                                  :pa (parse-integer (aref m 3) :radix 16)
                                  :size (parse-integer (aref m 5) :radix 16))
                mappings))))
    mappings))

;;; EOF

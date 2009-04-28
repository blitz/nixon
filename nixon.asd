;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(defpackage :blitz.debug.nixon.system
  (:use :common-lisp :asdf))
(in-package :blitz.debug.nixon.system)

(defsystem :nixon
  :depends-on (:defclass-star :cl-raw1394 :binary-types :iterate
                :cl-ppcre :gdb-remote :getopt)
  :description "A remote debugger framework."
  :version "0.0"
  :author "Julian Stecklina"
  :components ((:file "packages")
               (:file "x86-internals" :depends-on ("packages"))
               ;; XXX Not used at the moment
               #+ ignore (:file "elf" :depends-on ("packages"))

               (:file "target" :depends-on ("packages"))
               (:file "target-firewire" :depends-on ("target"))
               (:file "target-qemu" :depends-on ("target"))

               (:file "memory-cache" :depends-on ("packages"))
               (:file "control" :depends-on ("target" "x86-internals" "memory-cache"))
               (:file "gdb-server" :depends-on ("control"))
               (:file "gdb-breakpoints" :depends-on ("gdb-server"))
               (:file "gdb-query" :depends-on ("gdb-server" "gdb-breakpoints"))

               (:file "scan" :depends-on ("control"))

               (:file "main" :depends-on ("scan" "gdb-server" "target-qemu" "target-firewire"))

               ))

;;; EOF

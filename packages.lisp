;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(defpackage :blitz.debug.nixon
  (:use :common-lisp :defclass-star :binary-types :iterate
        :blitz.ffi.raw1394
        :blitz.debug.gdb-remote))
(in-package :blitz.debug.nixon)


;;; EOF

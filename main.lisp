;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.nixon)

(defparameter *gdb-server-default-port* 1235)
(defparameter *default-target-type* "firewire")

#+ ignore
(defun nova-server (&key 
                    (target (create-target *default-target-type*)))
  (let* ((n (make-instance 'controller 
                           :target target
                           :command-space-address (scan-for-command-space target :start #x420000 :step cs-align)))
         (s (make-instance 'nixon-server :controller n)))
    ;; Hack to scan for command space address at startup
    (command-space-address n)
    (values s n)))

(defun command-line-arguments ()
  #+ sbcl (rest sb-ext:*posix-argv*)
  #- sbcl (error "Port me!"))

(defun show-help ()
  (format t "
--port NUM    Open GDB server on port NUM (default ~A)
--target TYPE Specify target type. TYPE can be firewire or qemu (default ~A)
--fw-list     List all firewire nodes.
--cs-address ADDR
              Address (hexadecimal) of command space (or address where to start looking)
             (defaults to 0)
--cs-align NUM
              Alignment of command space in bytes (default 4096)
--help        Show this help
"
          *gdb-server-default-port*
          *default-target-type*))

(defun main ()
  ;; Print info.
  (let ((system (asdf:find-system :nixon)))
    (format t "~A ~A (on ~A ~A)~%" 
            (asdf:component-name system)
            (asdf:component-version system)
            (lisp-implementation-type)
            (lisp-implementation-version)))
  (handler-bind 
      ;; Bind handlers to various exceptions
      ((target-not-found 
        (lambda (c)
          (declare (ignore c))
          (format t "Target not found!~%")
          (return-from main 1)))
       (blitz.ffi.raw1394:raw1394-error 
        (lambda (c)
          (format t "Error: ~A~%" (class-name (class-of c)))
          (return-from main 1))))
    ;; Setup a SIGINT handler.
    #+ (and unix sbcl) (sb-sys:enable-interrupt sb-unix:sigint
                                                (lambda (&rest args)
                                                  (declare (ignore args))
                                                  (format t "~&Got SIGINT. Bye.~%")
                                                  (return-from main 1)))
    ;; Command line parsing
    (let ((port *gdb-server-default-port*)
          (target (list *default-target-type*))
          (cs-address 0)
          (cs-align 4096))
      (multiple-value-bind (args opts errors)
          (getopt:getopt (command-line-arguments)
                         '(("port" :required)
                           ("help" :none)
                           ("target" :required)
                           ("fw-list" :none)
                           ("cs-address" :required)
                           ("cs-align" :required)
                           ))
        (declare (ignore args))
        (labels ((strassoc (val)
                   (assoc val opts :test #'string=))
                 (num-arg (arg set-fn base)
                   (let ((p (cdr (strassoc arg))))
                     (when p
                       (handler-case
                           (funcall set-fn (parse-integer p :radix base))
                         (parse-error () 
                           (format t "Argument to ~A must be an integer." arg)
                           (return-from main 1)))))))
          ;; Errors on command line or help requested?
          (when (or errors (strassoc "help"))
            (show-help)
            (return-from main 
              (if errors 1 0)))
          ;; If no errors, interpret command line options
          ;; --fw-list
          (when (strassoc "fw-list")
            ;; Assume Firewire port 0
            (let ((fw-port (make-instance 'raw1394-port :port 0)))
              (unwind-protect
                   (raw1394-map-nodes fw-port (lambda (port node)
                                                (declare (ignore port))
                                                (format t "~&Node ~4,'0X: ~A~%" 
                                                        (raw1394-node-id node) 
                                                        (raw1394-node-guid node))))
                (raw1394-destroy fw-port))
              (return-from main 0)))
          ;; --port
          (num-arg "port" (lambda (v) (setq port v)) 10)
          ;; --cs-address
          (num-arg "cs-address" (lambda (v) (setq cs-address v)) 16)
          ;; --cs-align
          (num-arg "cs-align" (lambda (v) (setq cs-align v)) 10)
          ;; --target
          (setq target (or (cl-ppcre:split ":" (cdr (strassoc "target")))
                           target))
          (unless (find (first target) *target-names* :test #'string= :key #'first)
            (show-help)
            (return-from main 1))
          ;; put other arguments here
          ;; Run the damn thing. :)
          (let* ((target (create-target (first target)
                                        (second target)))
                 (cs (scan-for-command-space target :start cs-address :step cs-align))
                 (controller (make-instance 'controller
                                            :target target
                                            :command-space-address cs))
                 (server (make-instance 'nixon-server :controller controller)))
            (format t "Accepting GDB connections on port ~A.~%" port)
            (accept-gdb-connection server port)
            0))))))

;;; EOF

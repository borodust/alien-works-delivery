(cl:in-package :cl-user)

#+ecl
(require 'asdf)
(require 'uiop)

(declaim (special *target-directory*))

(import 'alien-works-delivery-util:shout)

(defparameter *target-directory* nil)
(defparameter *working-directory* nil)
(defparameter *bundle-directory* nil)


(defun deliver ()
  (alien-works-delivery-util:with-temporary-directory (:pathname work-dir)
    (let* ((bundle-type (first (uiop:command-line-arguments)))
           (root-dir (uiop:pathname-directory-pathname *load-pathname*))
           (target-dir root-dir)
           (bundle-dir (uiop:ensure-directory-pathname
                        (merge-pathnames
                         bundle-type
                         (merge-pathnames "bundles/" root-dir)))))
      (when (uiop:emptyp bundle-type)
        (error "Bundle type must be specified."))
      (shout "Delivering from ~A to ~A" bundle-dir target-dir)
      (setf *working-directory* (or (uiop:getenv "ALIEN_WORKS_DELIVERY_WORKDIR")
                                    work-dir)
            *target-directory* target-dir
            *bundle-directory* bundle-dir)
      (ensure-directories-exist *working-directory*)
      (ensure-directories-exist *target-directory*)
      (load (merge-pathnames "build.lisp" bundle-dir)))))


(defun run-delivery ()
  (unwind-protect
       (handler-case
           (deliver)
         (serious-condition (c)
           (shout "Delivery error: ~A~&" c)
           (uiop:quit 1 t))))
  (uiop:quit 0 t))


(run-delivery)

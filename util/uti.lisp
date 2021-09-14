(cl:defpackage :alien-works-delivery-util
  (:use :cl)
  (:export #:string*
           #:string+
           #:shell
           #:shout
           #:dir
           #:file
           #:ensure-unix-namestring
           #:with-temporary-directory
           #:provided-bundle-output-file))
(cl:defpackage :alien-works-delivery~pristine (:use))
(cl:in-package :alien-works-delivery-util)

(require 'asdf)
(require 'uiop)


(defvar *supress-errors* nil)
(defvar *shell-output* nil)


(macrolet ((%ensure-package (name)
             `(unless (find-package ,name)
                (uiop:ensure-package ,name))))
  (%ensure-package :sb-ext)
  (%ensure-package :ccl)
  (%ensure-package :alien-works-delivery-bundle))


(defun string* (control &rest args)
  (apply #'format nil control args))


(defun string+ (&rest args)
  (format nil "~{~A~}" args))


(defun shell (&rest args)
  (flet ((quote-arg (arg)
           (cond
             ((stringp arg) (string+ "\"" arg "\""))
             ((keywordp arg) (string* "~(~A~)" arg))
             ((pathnamep arg) (string+ "'" (uiop:native-namestring arg) "'"))
             (t (string arg)))))
    (let ((command (format nil "~{~A~^ ~}" (mapcar #'quote-arg args))))
      (multiple-value-bind (std err code)
          (uiop:run-program command :output (or *shell-output* :string)
                                    :error-output (unless *supress-errors*
                                                    *error-output*)
                                    :force-shell t
                                    :ignore-error-status t)
        (declare (ignore err))
        (if (= code 0)
            std
            (error "Shell command `~A` returned non-zero code (~A)" command code))))))


(defun shout (control &rest params)
  (handler-case
      (format t "~&~A~&" (apply #'format nil control params))
    (serious-condition (c)
      (warn "Failed to shout `~A` with arguments ~A: ~A" control params c)))
  (finish-output t))


(defun dir (base &rest pathnames)
  (flet ((ensure-relative-dir (dir)
           (uiop:ensure-directory-pathname (uiop:enough-pathname dir "/"))))
    (reduce #'merge-pathnames (nreverse (mapcar #'ensure-relative-dir pathnames))
            :initial-value (uiop:ensure-directory-pathname base)
            :from-end t)))


(defun file (&rest pathnames)
  (flet ((ensure-file (pathname)
           (let ((pathname (pathname pathname)))
             (if (uiop:directory-pathname-p pathname)
                 (let* ((dir (pathname-directory pathname))
                        (namepath (pathname (first (last dir)))))
                   (make-pathname :directory (butlast dir)
                                  :name (pathname-name namepath)
                                  :type (pathname-type namepath)
                                  :defaults pathname))
                 pathname))))
    (multiple-value-bind (neck last)
        (loop for (path . rest) on pathnames
              if rest
                collect path into neck
              else
                return (values neck (ensure-file path)))
      (if neck
          (merge-pathnames (uiop:enough-pathname last "/") (apply #'dir neck))
          last))))


(defun ensure-unix-namestring (path)
  (let ((filename (file-namestring path))
        (dirs (rest (pathname-directory path))))
    (format nil "~@[~A~]~{~A/~}~@[~A~]"
            (when (uiop:absolute-pathname-p path)
              (uiop:pathname-root path))
            dirs
            filename)))


(defmacro with-temporary-directory ((&key pathname) &body body)
  (let ((tmp-file (gensym))
        (tmp-dir (gensym)))
    `(uiop:with-temporary-file (:pathname ,tmp-file)
       (let* ((,tmp-dir (merge-pathnames (format nil "~A.dir/" (pathname-name ,tmp-file))
                                         (uiop:pathname-directory-pathname ,tmp-file)))
              ,@(when pathname
                  `((,pathname ,tmp-dir))))
         (unwind-protect
              (progn
                (ensure-directories-exist ,tmp-dir)
                ,@body)
           (uiop:delete-directory-tree ,tmp-dir :validate (constantly t)))))))


(defun provided-bundle-output-file ()
  (uiop:if-let (out (uiop:getenv "AWD_BUNDLE_FILE"))
    (file out)))

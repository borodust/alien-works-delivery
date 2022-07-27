(cl:defpackage :alien-works-delivery-util
  (:use :cl)
  (:export #:string*
           #:string+
           #:with-shell-configuration
           #:shell
           #:shout
           #:dir
           #:file
           #:cp
           #:rm
           #:ln
           #:mv
           #:compress
           #:decompress
           #:ensure-unix-namestring
           #:with-temporary-directory
           #:provided-bundle-output-file))
(cl:defpackage :alien-works-delivery~pristine (:use))
(cl:in-package :alien-works-delivery-util)

(require 'asdf)
(require 'uiop)


(defvar *supress-errors* nil)
(defvar *shell-output* nil)
(defvar *print-shell-command* nil)


(macrolet ((%ensure-package (name)
             `(unless (find-package ,name)
                (uiop:ensure-package ,name))))
  (%ensure-package :sb-ext)
  (%ensure-package :ccl)
  (%ensure-package :alien-works-delivery-bundle))


(defun windowsp ()
  (some #'uiop:featurep '(:windows :win32)))


(defun string* (control &rest args)
  (apply #'format nil control args))


(defun string+ (&rest args)
  (format nil "~{~A~}" args))


(defun verbatim-shell-string (string)
  (let ((escape-char (if (windowsp)
                         #\'
                         #\\)))
    (with-output-to-string (out)
      (format out "'")
      (loop for ch across string
            if (char= ch #\')
              do (format out "~A'" escape-char)
            else
              do (format out "~A" ch))
      (format out "'"))))


(defmacro with-shell-configuration ((&key current-directory
                                       supress-errors
                                       shell-output
                                       print-command)
                                    &body body)
  `(let (,@(when supress-errors
             `((*supress-errors* ,supress-errors)))
         ,@(when shell-output
             `((*shell-output* ,shell-output)))
         ,@(when print-command
             `((*print-shell-command* ,print-command))))
     (,@(if current-directory
            `(uiop:with-current-directory (,current-directory))
            '(progn))
      ,@body)))


(defun shell (command &rest args)
  (flet ((quote-arg (arg)
           (cond
             ((and (windowsp)
                   (stringp arg)
                   (> (length arg) 0)
                   (or (char= #\- (aref arg 0))
                       (char= #\" (aref arg 0))))
              arg)
             ((stringp arg) (verbatim-shell-string arg))
             ((keywordp arg) (string* "~(~A~)" arg))
             ((pathnamep arg) (if (windowsp)
                                  (string+ "\"" (uiop:native-namestring arg) "\"")
                                  (string+ "'" (uiop:native-namestring arg) "'")))
             (t (format nil "~A" arg)))))
    (let* ((command (format nil "~{~A ~}~{~A~^ ~}"
                            (if (windowsp)
                                (list "&" (verbatim-shell-string command))
                                (list (verbatim-shell-string command)))
                            (mapcar #'quote-arg args)))
           (shell (if (windowsp)
                      (list "powershell" "-Command")
                      (list "sh" "-c")))
           (full-command (nconc shell (list command))))
      (when *print-shell-command*
        (shout "Executing `~A`" full-command))
      (multiple-value-bind (std err code)
          (uiop:run-program full-command
                            :output (or *shell-output* :string)
                            :error-output (unless *supress-errors*
                                            *error-output*)
                            :force-shell nil
                            :ignore-error-status t)
        (declare (ignore err))
        (if (= code 0)
            std
            (error "Shell command `~A` returned non-zero code (~A)" command code))))))


(defun cp (destination source &rest sources)
  (ensure-directories-exist
   (uiop:pathname-directory-pathname destination))
  (if (windowsp)
      (shell "Copy-Item"
             "-LiteralPath" (format nil "~{\"~A\"~^,~}" (list* source sources))
             "-Destination" destination
             "-Recurse")
      (apply #'shell
             "/bin/cp" "-LR"
             (append
              (list* source sources)
              (list destination)))))


(defun mv (destination source)
  (ensure-directories-exist
   (uiop:pathname-directory-pathname destination))
  (if (windowsp)
      (shell "Move-Item"
             "-LiteralPath" source
             "-Destination" destination
             "-Force")
      (shell "/bin/mv" source destination)))


(defun compress (destination source)
  (if (windowsp)
      (let ((tmp-path (string+ source "~.zip")))
        (shell "Compress-Archive"
               "-LiteralPath" source
               "-DestinationPath" tmp-path
               "-Force")
        (mv destination tmp-path))
      (multiple-value-bind (parent source)
          (if (uiop:file-pathname-p source)
              (values (uiop:pathname-directory-pathname source) (file-namestring source))
              (let ((parent (uiop:pathname-parent-directory-pathname source)))
                (values parent (uiop:enough-pathname source parent))))
        (uiop:with-current-directory (parent)
          (shell "/bin/tar" "-czf" destination source)))))


(defun decompress (destination source)
  (let ((destination (uiop:ensure-directory-pathname destination)))
    (if (windowsp)
        (shell "Expand-Archive"
               "-Path" source
               "-DestinationPath" destination
               "-Force")
        (uiop:with-current-directory (destination)
          (shell "/bin/tar" "-xzf" source)))))


(defun rm (path)
  (if (windowsp)
      (shell "Remove-Item"
             "-LiteralPath" path
             "-Recurse"
             "-Force")
      (shell "/bin/rm" "-rf" path)))


(defun ln (destination source)
  (if (windowsp)
      (shell "New-Item"
             "-Path" destination
             "-ItemType" "SymbolicLink"
             "-Value" source
             "-Force")
      (shell "/bin/ln" "-s" source destination)))


(defun shout (control &rest params)
  (unwind-protect
       (handler-case
           (format *standard-output* "~&~A~&" (apply #'format nil control params))
         (serious-condition (c)
           (warn "Failed to shout `~A` with arguments ~A: ~A" control params c)))
    (finish-output *standard-output*)))


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
  (uiop:if-let (out (uiop:getenv "ALIEN_WORKS_DELIVERY_BUNDLE_TARGET_FILE"))
    (file out)))

(require 'uiop)
(declaim (special cl-user::*target-directory*))
#!
(uiop:with-temporary-file (:pathname tmp-archive-file
                           :stream tmp-archive-stream
                           :direction :output
                           :element-type '(unsigned-byte 8))
  (uiop:copy-stream-to-stream *rest-stream*
                              tmp-archive-stream :element-type '(unsigned-byte 8))
  :close-stream
  (uiop:with-temporary-file (:pathname tmp-work-dir)
    (let ((tmp-work-dir (uiop:ensure-directory-pathname
                         (format nil "~A.dir/" (namestring tmp-work-dir)))))
      (ensure-directories-exist tmp-work-dir)
      (unwind-protect
           (uiop:with-current-directory (tmp-work-dir)
             (uiop:run-program (list "tar" "-xzf" (namestring tmp-archive-file)))
             (let ((cl-user::*target-directory* alien-works-delivery-bundle:*bundle-pathname*))
              (load (merge-pathnames "delivery-bundle/build.lisp" tmp-work-dir))))
        (uiop:delete-directory-tree tmp-work-dir :validate (constantly t)))))
  (cl-user::quit))
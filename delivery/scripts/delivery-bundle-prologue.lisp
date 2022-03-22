#+ecl
(require 'asdf)
(require 'uiop)
(declaim (special cl-user::*target-directory*))

(defun shout (control &rest args)
  (format *standard-output* "~&")
  (apply #'format *standard-output* control args)
  (finish-output *standard-output*))

(defun unpack-archive (target-dir archive-file)
  (if (some #'uiop:featurep '(:windows :win32))
      (uiop:run-program
       (list "powershell" "-Command"
             (format nil "Expand-Archive -Path \"~A\" -DestinationPath \"~A\" -Force"
                     (namestring archive-file)
                     (namestring target-dir)))
       :force-shell nil
       :error-output *error-output*)
      (uiop:with-current-directory (target-dir)
        (uiop:run-program (list "tar" "-xzf" (namestring archive-file))
                          :error-output *error-output*))))


(defun print-builder (builder-file work-dir)
  (with-open-file (out builder-file :direction :output :if-exists :supersede)
    (prin1 '(require 'asdf) out)
    (prin1 '(require 'uiop) out)
    (prin1 `(progn
              (defparameter
                  cl-user::*target-directory*
                ,alien-works-delivery-bundle:*bundle-pathname*)
              (handler-case
                  (load ,(merge-pathnames "delivery-bundle/build.lisp"
                                          work-dir))
                (serious-condition ()
                  (uiop:quit 1 t)))
              (uiop:quit 0 t))
           out)))


(defun deliver (work-dir)
  (let* (error
         (out (with-output-to-string (err-out)
                (handler-case
                    (uiop:with-temporary-file (:pathname builder-file)
                      (print-builder builder-file work-dir)
                      (uiop:run-program
                       (list (first (uiop:raw-command-line-arguments))
                             "--load" (namestring builder-file))
                       :output *standard-output*
                       :error-output err-out))
                  (serious-condition (c)
                    (finish-output err-out)
                    (setf error c)))
                (finish-output *standard-output*))))
    (values error out)))


(defun process-embedded-delivery-bundle (tmp-archive-file)
  (uiop:with-temporary-file (:pathname tmp-work-dir)
    (let* ((provided-dir (uiop:getenv "ALIEN_WORKS_DELIVERY_UNPACK_DIR"))
           (extract-only-p (not (uiop:emptyp (uiop:getenv "ALIEN_WORKS_DELIVERY_EXTRACT_ONLY"))))
           (keep-extracted-p (or extract-only-p
                                 (not (uiop:emptyp
                                       (uiop:getenv "ALIEN_WORKS_DELIVERY_KEEP_EXTRACTED")))))
           (tmp-work-dir (uiop:ensure-directory-pathname
                          (or (unless (uiop:emptyp provided-dir)
                                provided-dir)
                              (format nil "~A.dir/" (namestring tmp-work-dir))))))
      (ensure-directories-exist tmp-work-dir)
      (handler-case
          (unwind-protect
               (progn
                 (unpack-archive tmp-work-dir tmp-archive-file)
                 (shout "~%Embedded archive unpacked at ~A."
                        tmp-work-dir)
                 (unless extract-only-p
                   (shout "Delivering.")
                   (multiple-value-bind (error errout)
                       (deliver tmp-work-dir)
                     (when error
                       (error "~A: ~A" error errout)))))
            (unless keep-extracted-p
              (uiop:delete-directory-tree tmp-work-dir :validate (constantly t))))
        (serious-condition (c)
          (format *standard-output* "~%Bundle error: ~A~&" c)
          (finish-output *standard-output*)
          (uiop:quit 1 t)))
      (uiop:quit 0 t))))

#!
(uiop:with-temporary-file (:pathname tmp-archive-file
                           :stream tmp-archive-stream
                           :direction :output
                           :type #+(or windows win32) "zip" #-(or windows win32) "tmp"
                           :element-type '(unsigned-byte 8))
  (uiop:copy-stream-to-stream *rest-stream*
                              tmp-archive-stream :element-type '(unsigned-byte 8))
  :close-stream
  (process-embedded-delivery-bundle tmp-archive-file))
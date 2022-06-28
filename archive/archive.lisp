(cl:defpackage :alien-works-delivery.archive
  (:use :cl :alien-works-delivery-util)
  (:local-nicknames (:awd :alien-works-delivery)
                    (:awdu :alien-works-delivery-util)))
(cl:in-package :alien-works-delivery.archive)


(defclass archive-bundle ()
  ((output-filename :initarg :output-filename)
   (name :initarg :name)))


(defmethod awd:make-delivery-bundle ((type (eql :archive)) bundle-def)
  (let ((output-name (substitute #\- #\/
                                 (asdf:component-name
                                  (asdf:find-system
                                   (awd:bundle-system-name bundle-def))))))
    (make-instance 'archive-bundle
                   :name output-name
                   :output-filename (format nil
                                            #+windows "~A.zip"
                                            #-windows "~A.tar.gz"
                                            output-name))))


(defmethod awd:prepare-delivery-bundle ((bundle archive-bundle))
  (with-slots (name) bundle
    (let ((bundle-dir (dir awd:*delivery-bundle-directory* name)))
      (flet ((%appdir-path (relative)
               (merge-pathnames (uiop:relativize-pathname-directory relative) bundle-dir))
             (%system-path (relative)
               (asdf:system-relative-pathname :alien-works-delivery/archive
                                              (uiop:relativize-pathname-directory relative))))
        (let* ((bin-dir (%appdir-path "./")))
          (ensure-directories-exist bin-dir)
          (ensure-directories-exist (%appdir-path "lib/")))))))


(defmethod awd:delivery-bundle-foreign-library-directory ((bundle archive-bundle))
  (with-slots (name) bundle
    (dir name "lib/")))


(defmethod awd:delivery-bundle-asset-directory ((bundle archive-bundle))
  (with-slots (name) bundle
    (dir name "rsc/")))


(defmethod awd:delivery-bundle-executable-path ((bundle archive-bundle))
  (with-slots (name) bundle
    (file name
          #+windows "/app.exe"
          #-windows "/app.bin")))


(defmethod awd:delivery-bundle-assembler-parameters ((bundle archive-bundle))
  (with-slots (output-filename name) bundle
    (list :bundler-output-filename output-filename
          :bundler-name name)))


(defmethod awd:write-delivery-bundle-assembler-source ((bundle archive-bundle) stream)
  (alexandria:with-input-from-file (in (asdf:system-relative-pathname
                                        :alien-works-delivery/archive
                                        "archive/scripts/bundler.lisp"))
    (uiop:copy-stream-to-stream in stream)))


(defmethod awd:delivery-bundle-build-features ((bundle archive-bundle))
  '(:awd-archive))

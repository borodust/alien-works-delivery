(cl:defpackage :alien-works-delivery.msix
  (:use :cl)
  (:local-nicknames (:awd :alien-works-delivery)
                    (:awdu :alien-works-delivery-util)))
(cl:in-package :alien-works-delivery.msix)


(defclass msix-bundle ()
  ((output-filename :initarg :output-filename)))


(defmethod awd:make-delivery-bundle ((type (eql :msix)) bundle-def &key &allow-other-keys)
  (let ((output-name (substitute #\- #\/
                                 (asdf:component-name
                                  (asdf:find-system
                                   (awd:bundle-system-name bundle-def))))))
    (make-instance 'msix-bundle
                   :output-filename (format nil "~A.msix" output-name))))


(defmethod awd:prepare-delivery-bundle ((bundle msix-bundle))
  (let ((bundle-dir (merge-pathnames "AppDir/" awd:*delivery-bundle-directory*)))
    (flet ((%appdir-path (relative)
             (merge-pathnames (uiop:relativize-pathname-directory relative) bundle-dir))
           (%system-path (relative)
             (asdf:system-relative-pathname :alien-works-delivery/msix
                                            (uiop:relativize-pathname-directory relative))))
      (let* ((bin-dir (%appdir-path "./"))
             (scalable-icon-dir (%appdir-path "img/"))
             (metadata-dir (%appdir-path "./"))
             (metadata-file (merge-pathnames "appxmanifest.xml" metadata-dir)))
        (ensure-directories-exist bin-dir)
        (ensure-directories-exist (%appdir-path "lib/"))
        (ensure-directories-exist scalable-icon-dir)
        (ensure-directories-exist metadata-dir)

        (awdu:cp metadata-file (%system-path "msix/templates/appxmanifest.xml"))
        (awdu:cp scalable-icon-dir
                 (%system-path "msix/templates/app150.png")
                 (%system-path "msix/templates/app44.png"))))))


(defmethod awd:delivery-bundle-foreign-library-directory ((bundle msix-bundle))
  #P"AppDir/lib/")


(defmethod awd:delivery-bundle-asset-directory ((bundle msix-bundle))
  #P"AppDir/rsc/")


(defmethod awd:delivery-bundle-executable-path ((bundle msix-bundle))
  #P"AppDir/app.exe")


(defmethod awd:delivery-bundle-assembler-parameters ((bundle msix-bundle))
  (with-slots (output-filename) bundle
    (list :bundler-output-filename output-filename)))


(defmethod awd:write-delivery-bundle-assembler-source ((bundle msix-bundle) stream)
  (alexandria:with-input-from-file (in (asdf:system-relative-pathname
                                        :alien-works-delivery/msix
                                        "msix/scripts/bundler.lisp"))
    (uiop:copy-stream-to-stream in stream)))


(defmethod awd:delivery-bundle-build-features ((bundle msix-bundle))
  '(:msix))

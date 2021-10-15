(cl:defpackage :alien-works-delivery.appimage
  (:use :cl)
  (:local-nicknames (:awd :alien-works-delivery)
                    (:awdu :alien-works-delivery-util)))
(cl:in-package :alien-works-delivery.appimage)


(defclass appimage-bundle ()
  ((output-filename :initarg :output-filename)))


(defmethod awd:make-delivery-bundle ((type (eql :appimage)) bundle-def &key &allow-other-keys)
  (let ((output-name (substitute #\- #\/
                                 (asdf:component-name
                                  (asdf:find-system
                                   (awd:bundle-system-name bundle-def))))))
    (make-instance 'appimage-bundle
                   :output-filename (format nil "~A.AppImage" output-name))))


(defmethod awd:prepare-delivery-bundle ((bundle appimage-bundle))
  (let ((bundle-dir (merge-pathnames "AppDir/" awd:*delivery-bundle-directory*)))
    (flet ((%appdir-path (relative)
             (merge-pathnames (uiop:relativize-pathname-directory relative) bundle-dir))
           (%system-path (relative)
             (asdf:system-relative-pathname :alien-works-delivery/appimage
                                            (uiop:relativize-pathname-directory relative))))
      (let* ((bin-dir (%appdir-path "usr/bin/"))
             (desktop-dir (%appdir-path "usr/share/applications/"))
             (desktop-file (merge-pathnames "app.desktop" desktop-dir))
             (scalable-icon-dir (%appdir-path "usr/share/icons/hicolor/scalable/apps/"))
             (scalable-icon-file (merge-pathnames "app.svg" scalable-icon-dir))
             (metadata-dir (%appdir-path "usr/share/metainfo/"))
             (metadata-file (merge-pathnames "app.appdata.xml" metadata-dir))
             (apprun-file (%appdir-path "AppRun")))
        (ensure-directories-exist bin-dir)
        (ensure-directories-exist (%appdir-path "usr/lib/"))
        (ensure-directories-exist desktop-dir)
        (ensure-directories-exist scalable-icon-dir)
        (ensure-directories-exist metadata-dir)

        (awdu:cp desktop-file (%system-path "appimage/templates/app.desktop.template"))
        (awdu:cp scalable-icon-file (%system-path "appimage/templates/app.svg"))
        (awdu:cp metadata-file (%system-path "appimage/templates/app.metainfo.xml"))
        (awdu:cp apprun-file (%system-path "appimage/templates/AppRun"))

        (awdu:ln (%appdir-path "app.desktop")
                 (enough-namestring desktop-file bundle-dir))
        (awdu:ln (%appdir-path "app.svg")
                 (enough-namestring scalable-icon-file bundle-dir))))))


(defmethod awd:delivery-bundle-foreign-library-directory ((bundle appimage-bundle))
  #P"AppDir/usr/lib/")


(defmethod awd:delivery-bundle-asset-directory ((bundle appimage-bundle))
  #P"AppDir/usr/share/app/")


(defmethod awd:delivery-bundle-executable-path ((bundle appimage-bundle))
  #P"AppDir/usr/bin/app")


(defmethod awd:delivery-bundle-assembler-parameters ((bundle appimage-bundle))
  (with-slots (output-filename) bundle
    (list :bundler-output-filename output-filename)))


(defmethod awd:write-delivery-bundle-assembler-source ((bundle appimage-bundle) stream)
  (alexandria:with-input-from-file (in (asdf:system-relative-pathname
                                        :alien-works-delivery/appimage
                                        "appimage/scripts/bundler.lisp"))
    (uiop:copy-stream-to-stream in stream)))


(defmethod awd:delivery-bundle-build-features ((bundle appimage-bundle))
  '(:appimage))

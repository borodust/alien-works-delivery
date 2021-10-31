(cl:defpackage :alien-works-delivery.android
  (:use :cl)
  (:local-nicknames (:awd :alien-works-delivery)
                    (:awdu :alien-works-delivery-util)))
(cl:in-package :alien-works-delivery.android)


(defclass android-bundle ()
  ((output-filename :initarg :output-filename)))


(defmethod awd:make-delivery-bundle ((type (eql :android)) bundle-def &key &allow-other-keys)
  (let ((output-name (substitute #\- #\/
                                 (asdf:component-name
                                  (asdf:find-system
                                   (awd:bundle-system-name bundle-def))))))
    (make-instance 'android-bundle
                   :output-filename (format nil "~A.apk" output-name))))


(defmethod awd:prepare-delivery-bundle ((bundle android-bundle))
  (flet ((%system-path (relative)
           (asdf:system-relative-pathname :alien-works-delivery/android
                                          (merge-pathnames
                                           (uiop:relativize-pathname-directory relative)
                                           "android/"))))
    (awdu:cp (awdu:dir awd:*delivery-bundle-directory* "AppDir/")
             (%system-path "template/"))))


(defmethod awd:delivery-bundle-foreign-library-directory ((bundle android-bundle))
  #P"AppDir/app/lib/arm64-v8a/")


(defmethod awd:delivery-bundle-asset-directory ((bundle android-bundle))
  #P"AppDir/app/src/main/assets/")


(defmethod awd:delivery-bundle-executable-path ((bundle android-bundle))
  #P"./")


(defmethod awd:delivery-bundle-assembler-parameters ((bundle android-bundle))
  (with-slots (output-filename) bundle
    (list :bundler-output-filename output-filename
          :bundler-android-sdk nil)))


(defmethod awd:write-delivery-bundle-assembler-source ((bundle android-bundle) stream)
  (alexandria:with-input-from-file (in (asdf:system-relative-pathname
                                        :alien-works-delivery/android
                                        "android/scripts/bundler.lisp"))
    (uiop:copy-stream-to-stream in stream)))


(defmethod awd:delivery-bundle-build-features ((bundle android-bundle))
  '(:android :alien-works-delivery-android))

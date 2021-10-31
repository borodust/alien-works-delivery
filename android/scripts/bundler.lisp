(cl:in-package :cl-user)

;;
;; INPUT:
;;   *bundler-output-filename*
;;   *bundler-android-sdk*
;;
;; PROVIDED VIA BUILD:
;;   *target-bundle-directory*
;;   *delivery-bundle-directory*
;;   *builder-implementation*
;;
(flet ((%bundle-path (relative)
         (merge-pathnames (uiop:relativize-pathname-directory relative)
                          *target-bundle-directory*)))
  (let* ((app-dir (merge-pathnames "AppDir/" *delivery-bundle-directory*))
         (sdk-dir (or *bundler-android-sdk*
                      (uiop:getenv "ALIEN_WORKS_DELIVERY_ANDROID_SDK")
                      (uiop:getenv "ANDROID_SDK_ROOT")))
         (source-apk-file (file app-dir "app/build/outputs/apk/release/app-release-unsigned.apk"))
         (target-file (or (provided-bundle-output-file)
                          (merge-pathnames
                           *bundler-output-filename*
                           (%bundle-path "./")))))

    (when (eq *builder-implementation* :lispworks)
      (let ((heap-file (first (directory
                               (file *delivery-bundle-directory*
                                     "../libLispWorks*.lwheap"))) )
            (shared-library (first (directory
                                    (file *delivery-bundle-directory*
                                          "../libLispWorks*.so")))))
        (shout "Copying LispWorks heap (~A) and shared library (~A)" heap-file shared-library)
        (cp (dir app-dir "app/lib/arm64-v8a/") shared-library)
        (cp (dir app-dir "app/src/main/assets/") heap-file)))
    (shout "Building .APK in ~A using Android SDK from `~A`." app-dir sdk-dir)
    (setf (uiop:getenv "ANDROID_SDK_ROOT") sdk-dir)
    (with-shell-configuration (:shell-output *standard-output*
                               :current-directory app-dir)
      (shell "./gradlew" "build")
      (finish-output *standard-output*))
    (shout "Moving .APK from `~A` to `~A`." source-apk-file target-file)
    (cp target-file source-apk-file)))

(cl:in-package :cl-user)

;;
;; INPUT:
;;   *bundler-output-filename*
;;
(flet ((%system-path (relative)
         (merge-pathnames (uiop:relativize-pathname-directory relative)
                          *target-bundle-directory*)))
  (let ((app-dir (merge-pathnames "AppDir/" *delivery-bundle-directory*)))
    (shell "chmod" "+x" (file app-dir "AppRun"))
    (shell "appimagetool" "--no-appstream"
           app-dir
           (or (provided-bundle-output-file)
               (merge-pathnames
                *bundler-output-filename*
                (%system-path "./"))))))

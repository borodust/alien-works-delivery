(cl:in-package :cl-user)

;;
;; INPUT:
;;   *bundler-output-filename*
;;
(flet ((%system-path (relative)
         (merge-pathnames (uiop:relativize-pathname-directory relative)
                          *target-bundle-directory*)))
  (shell "appimagetool" "--no-appstream"
         (merge-pathnames "AppDir/" *delivery-bundle-directory*)
         (or (provided-bundle-output-file)
             (merge-pathnames
              *bundler-output-filename*
              (%system-path "./")))))

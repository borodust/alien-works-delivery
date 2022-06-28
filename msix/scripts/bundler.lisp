(cl:in-package :cl-user)

;;
;; INPUT:
;;   *bundler-output-filename*
;;
(flet ((%system-path (relative)
         (merge-pathnames (uiop:relativize-pathname-directory relative)
                          *target-bundle-directory*)))
  (cp *delivery-bundle-directory* (merge-pathnames "AppDir/" *bundle-directory*))
  (let* ((app-dir (merge-pathnames "AppDir/" *delivery-bundle-directory*))
         (target-bundle-file (or (provided-bundle-output-file)
                                 (merge-pathnames
                                  *bundler-output-filename*
                                  (%system-path "./")))))
    (shout "Packing ~A" app-dir)
    (shell "MakeAppx" "pack"
           "/d" app-dir
           "/p" target-bundle-file)
    (shout "Bundle created at ~A." target-bundle-file)))

(cl:in-package :cl-user)

;;
;; INPUT:
;;   *bundler-output-filename*
;;   *bundler-name*
;;
(flet ((%system-path (relative)
         (merge-pathnames (uiop:relativize-pathname-directory relative)
                          *target-bundle-directory*)))
  (let* ((app-dir (merge-pathnames *bundler-name* *delivery-bundle-directory*))
         (target-bundle-file (or (provided-bundle-output-file)
                                 (merge-pathnames
                                  *bundler-output-filename*
                                  (%system-path "./")))))
    (shout "Compressing ~A" app-dir)
    (compress target-bundle-file app-dir)
    (shout "Archive created at ~A." target-bundle-file)))

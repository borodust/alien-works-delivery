(cl:in-package :cl-user)

;;
;; INPUT:
;;   *bundler-output-filename*
;;   *bundler-name*
;;
(flet ((%system-path (relative)
         (merge-pathnames (uiop:relativize-pathname-directory relative)
                          *target-bundle-directory*)))
  (let* ((windows-p (uiop:featurep '(:or :windows :os-windows)))
         (app-dir (merge-pathnames *bundler-name* *delivery-bundle-directory*))
         (target-bundle-file (or (provided-bundle-output-file)
                                 (format nil "~A.~A"
                                         (merge-pathnames
                                          *bundler-output-filename*
                                          (%system-path "./"))
                                         (if windows-p "zip" "tar.gz")))))
    (when windows-p
      (mv (file app-dir "app.exe") (file app-dir "app.bin")))
    (shout "Compressing ~A" app-dir)
    (compress target-bundle-file app-dir)
    (shout "Archive created at ~A." target-bundle-file)))

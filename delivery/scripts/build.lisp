(cl:in-package :cl-user)

(use-package :alien-works-delivery-util)

;;
;; INPUT:
;;   *bundle-executable-path*
;;
(declaim (special *delivery-bundle-directory*
                  *target-bundle-directory*))

(let ((work-dir (uiop:pathname-directory-pathname *load-pathname*))
      (target-directory (uiop:pathname-directory-pathname cl-user::*target-directory*)))
  (apply #'shell (first (uiop:raw-command-line-arguments))
         (append
          (cond
            ((uiop:featurep :ccl) (list "--no-init"))
            ((uiop:featurep :sbcl) (list "--no-userinit"))
            ((uiop:featurep :ecl) (list "--norc")))
          (list "--load" (merge-pathnames "registry/registry.lisp" work-dir)
                "--load" (merge-pathnames "builder.lisp" work-dir))))
  (let ((*delivery-bundle-directory* (dir work-dir "bundle/"))
        (*target-bundle-directory* target-directory))
    (load (merge-pathnames "blobs.lisp" work-dir))
    (shell "cp" "-L"
           (file work-dir "app.bin")
           (file *delivery-bundle-directory* *bundle-executable-path*))
    (load (merge-pathnames "bundler.lisp" work-dir))))

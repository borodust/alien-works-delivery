(cl:in-package :cl-user)

(use-package :alien-works-delivery-util)

;;
;; INPUT:
;;   *bundle-executable-path*
;;
;; PROVIDED VIA BUNDLE:
;;   *target-directory*
;;
(declaim (special *delivery-bundle-directory*
                  *target-bundle-directory*))

(let* ((work-dir (uiop:pathname-directory-pathname *load-pathname*))
       (target-directory (uiop:pathname-directory-pathname *target-directory*))
       (*load-verbose* nil)
       (*compile-verbose* nil)
       (*load-print* nil)
       (*compile-print* nil)
       (asdf:*user-cache* (merge-pathnames
                           ".cache/"
                           (uiop:pathname-directory-pathname *load-pathname*)))
       (registry-path (merge-pathnames "registry/registry.lisp" work-dir))
       (*delivery-bundle-directory* (dir work-dir "bundle/"))
       (*target-bundle-directory* target-directory)
       (target-executable (file *delivery-bundle-directory* *bundle-executable-path*)))
  (shout "Loading registry.")
  (load registry-path)
  (shout "Building executable.")
  (apply #'shell (first (uiop:raw-command-line-arguments))
         (append
          (cond
            ((uiop:featurep :ccl) (list "--no-init"))
            ((uiop:featurep :sbcl) (list "--no-userinit"))
            ((uiop:featurep :ecl) (list "--norc")))
          (list "--load" registry-path
                "--load" (merge-pathnames "builder.lisp" work-dir))))
  (shout "Moving executable to ~A" target-executable)
  (mv target-executable (file work-dir "app.bin"))
  (shout "Preparing foreign libraries.")
  (load (merge-pathnames "blobs.lisp" work-dir))

  (when (uiop:featurep :unix)
    (shout "Ensure executable.")
    (shell "chmod" "+x" target-executable))
  (shout "Bundling.")
  (load (merge-pathnames "bundler.lisp" work-dir))
  (shout "Done."))

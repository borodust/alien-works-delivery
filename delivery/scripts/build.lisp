(cl:in-package :cl-user)

(use-package :alien-works-delivery-util)

;;
;; INPUT:
;;   *bundle-executable-path*
;;
;; PROVIDED VIA BUNDLE:
;;   *target-directory*
;;   *working-directory*
;;   *bundle-directory*
;;
(defun provided-target-features ()
  (flet ((make-keyword (name)
           (intern name :keyword)))
    (mapcar #'make-keyword
            (mapcar #'uiop:standard-case-symbol-name
                    (remove-if #'uiop:emptyp
                               (uiop:split-string
                                (uiop:getenv "ALIEN_WORKS_DELIVERY_TARGET_FEATURES") :separator " "))))))

(declaim (special *delivery-bundle-directory*
                  *target-bundle-directory*
                  *builder-implementation*
                  *bundle-executable-path*
                  *target-features*))

(with-shell-configuration (:print-command t
                           :shell-output *standard-output*
                           :current-directory *working-directory*)
  (let* ((work-dir *working-directory*)
         (bundle-dir *bundle-directory*)
         (target-directory (uiop:ensure-directory-pathname *target-directory*))
         (*load-verbose* nil)
         (*compile-verbose* nil)
         (*load-print* nil)
         (*compile-print* nil)
         (asdf:*user-cache* (merge-pathnames ".cache/" work-dir))
         (registry-path (merge-pathnames "../../registry/registry.lisp"
                                         *bundle-directory*))
         (*delivery-bundle-directory* (dir work-dir "bundle/"))
         (*target-bundle-directory* target-directory)
         (target-executable-path (when *bundle-executable-path*
                                   (merge-pathnames *bundle-executable-path*
                                                    *delivery-bundle-directory*)))
         (provided-impl (string-downcase (uiop:getenv "ALIEN_WORKS_DELIVERY_LISP_IMPLEMENTATION")))
         (*builder-implementation* (cond
                                     ((string= provided-impl "ccl") :ccl)
                                     ((string= provided-impl "sbcl") :sbcl)
                                     ((string= provided-impl "ecl") :ecl)
                                     ((string= provided-impl "lispworks") :lispworks)
                                     (t (cond
                                          ((uiop:featurep :ccl) :ccl)
                                          ((uiop:featurep :sbcl) :sbcl)
                                          ((uiop:featurep :ecl) :ecl)
                                          ((uiop:featurep :lispworks) :lispworks)
                                          (t (error "Unsupported builder implementation: ~A"
                                                    (uiop:implementation-identifier))))))))

    (shout "Loading registry.")
    (load registry-path)

    (when (asdf:find-system :trivial-features nil)
      (asdf:load-system :trivial-features))

    (let ((*target-features* (or (provided-target-features) *features*)))
      (shout "Target features: ~{~A~^, ~}" *target-features*)

      (shout "Building executable.")
      (apply #'shell (or
                      (uiop:getenv "ALIEN_WORKS_DELIVERY_LISP")
                      (first (uiop:raw-command-line-arguments)))
             (append
              (ecase *builder-implementation*
                (:ccl (list "--no-init"))
                (:sbcl (list "--no-userinit"))
                (:ecl (list "--norc"))
                (:lispworks (list "-init" "-")))
              (if (eq *builder-implementation* :lispworks)
                  (list "-eval" "(lispworks::load-all-patches)"
                        "-load" registry-path
                        "-build" (merge-pathnames "builder.lisp" bundle-dir))
                  (list "--load" registry-path
                        "--load" (merge-pathnames "builder.lisp" bundle-dir)))))
      (when (uiop:file-pathname-p target-executable-path)
        (shout "Moving executable to ~A" target-executable-path)
        (mv target-executable-path (file work-dir "app.bin"))
        (when (uiop:featurep :unix)
          (shout "Ensure executable.")
          (shell "chmod" "+x" target-executable-path)))

      (shout "Preparing foreign libraries.")
      (load (merge-pathnames "blobs.lisp" bundle-dir))

      (shout "Bundling.")
      (load (merge-pathnames "bundler.lisp" bundle-dir))
      (shout "Done."))))

(cl:in-package :cl-user)

;;
;; INPUT:
;;   *runner-symbol*
;;   *base-system-name*
;;
(let ((*load-verbose* nil)
      (*compile-verbose* nil)
      (*load-print* nil)
      (*compile-print* nil)
      (asdf:*user-cache* (merge-pathnames
                          ".cache/"
                          (uiop:pathname-directory-pathname *load-pathname*))))
  (uiop:with-muffled-conditions ('(cl:warning))
    (uiop:with-muffled-compiler-conditions ()
      (uiop:with-muffled-loader-conditions ()
        (asdf:load-system *base-system-name*)))))
(setf uiop:*image-entry-point* (apply #'uiop:find-symbol* *runner-symbol*))
(pushnew :appimage *features*)
(apply #'uiop:dump-image
       (merge-pathnames "app.bin"
                        (uiop:pathname-directory-pathname *load-pathname*))
       :executable t
       (append
        (when (uiop:featurep :clozure)
          (list :purify t))
        (when (uiop:featurep :sbcl)
          (list :compression 9))
        (when (and (uiop:featurep :sbcl)
                   (uiop:featurep :os-windows))
          (list :application-type :gui))))

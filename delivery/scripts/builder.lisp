(cl:in-package :cl-user)

;;
;; INPUT:
;;   *runner-symbol*
;;   *base-system-name*
;;
(asdf:load-system *base-system-name*)
(setf uiop:*image-entry-point* (apply #'uiop:find-symbol* *runner-symbol*))
(pushnew :appimage *features*)
(apply #'uiop:dump-image
       (merge-pathnames "app.bin"
                        (uiop:pathname-directory-pathname *load-pathname*))
       :executable t
       :purify t
       (when (uiop:featurep :sbcl)
         (list* :compression 9
                (when (uiop:featurep :win32)
                  (list :application-type :gui)))))

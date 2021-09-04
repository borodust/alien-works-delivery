(cl:in-package :cl-user)

;;
;; INPUT:
;;   *runner-symbol*
;;   *delivery-bundle-features*
;;   *base-system-name*
;;

(flet ((runner-symbol ()
         (destructuring-bind (symbol-name &optional package-name)
             (reverse (uiop:ensure-list *runner-symbol*))
           (intern (format nil "~A" symbol-name)
                   (uiop:ensure-package (or package-name :cl-user))))))
  (let ((*load-verbose* nil)
        (*compile-verbose* nil)
        (*load-print* nil)
        (*compile-print* nil)
        (asdf:*user-cache* (merge-pathnames
                            ".cache/"
                            (uiop:pathname-directory-pathname *load-pathname*)))
        (target-path (merge-pathnames "app.bin"
                                      (uiop:pathname-directory-pathname *load-pathname*))))

    (uiop:with-muffled-conditions ('(cl:warning))
      (uiop:with-muffled-compiler-conditions ()
        (uiop:with-muffled-loader-conditions ()
          (asdf:load-system *base-system-name*)

          #+ecl
          (let ((result (first
                         (asdf:make-build *base-system-name*
                                          :type :program
                                          :prologue-code `(progn
                                                            (setf *features*
                                                                  (append ',*delivery-bundle-features*
                                                                          *features*)))
                                          :epilogue-code `(progn
                                                            (,(runner-symbol)))
                                          :move-here (uiop:pathname-directory-pathname
                                                      *load-pathname*)))))
            (uiop:rename-file-overwriting-target result target-path))

          #-ecl
          (progn
            (setf *features* (append *delivery-bundle-features* *features*))
            (setf uiop:*image-entry-point* (runner-symbol))
            (apply #'uiop:dump-image
                   target-path
                   :executable t
                   (append
                    (when (uiop:featurep :clozure)
                      (list :purify t))
                    (when (uiop:featurep :sbcl)
                      (list :compression 9))
                    (when (and (uiop:featurep :sbcl)
                               (uiop:featurep :os-windows))
                      (list :application-type :gui))))))))))

(uiop:quit 0)

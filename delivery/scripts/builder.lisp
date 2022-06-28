(cl:in-package :cl-user)

;;
;; INPUT:
;;   *runner-symbol*
;;   *delivery-bundle-features*
;;   *base-system-name*
;;

(handler-case
    (flet ((runner-symbol ()
             (destructuring-bind (symbol-name &optional package-name)
                 (reverse (uiop:ensure-list *runner-symbol*))
               (intern (format nil "~A" symbol-name)
                       (uiop:ensure-package (or package-name :cl-user))))))
      (let* ((*load-verbose* nil)
             (*compile-verbose* nil)
             (*load-print* nil)
             (*compile-print* nil)
             (work-dir (uiop:getcwd))
             (asdf:*user-cache* (merge-pathnames ".cache/" work-dir))
             (target-path (merge-pathnames "app.bin" work-dir)))

        (uiop:with-muffled-conditions ('(cl:warning))
          (uiop:with-muffled-compiler-conditions ()
            (uiop:with-muffled-loader-conditions ()
              (pushnew :bodge-blobs-support-no-preload *features*)
              (asdf:load-system *base-system-name* :verbose nil)
              (setf asdf:*central-registry* nil
                    asdf:*user-cache* nil)

              #+ecl
              (let ((result
                      (first
                       (asdf:make-build
                        *base-system-name*
                        :type :program
                        :prologue-code `(progn
                                          (setf *features*
                                                (remove-duplicates
                                                 (append ',*delivery-bundle-features*
                                                         *features*))))
                        :epilogue-code `(progn
                                          (,(runner-symbol)))
                        :move-here work-dir))))
                (uiop:rename-file-overwriting-target result target-path))

              #+(and lispworks android-delivery)
              (progn
                (setf *features* (append *delivery-bundle-features* *features*))
                (hcl:deliver-to-android-project nil
                                                work-dir
                                                0
                                                :no-sub-dir t))

              #-(or ecl (and lispworks android-delivery))
              (progn
                (setf *features* (remove-duplicates
                                  (append *delivery-bundle-features* *features*))
                      uiop:*image-entry-point* (runner-symbol))
                uiop:*image-entry-point*
                (apply #'uiop:dump-image
                       target-path
                       :executable t
                       (append
                        (when (uiop:featurep :clozure)
                          (list :purify t))
                        (when (and (uiop:featurep :sbcl)
                                   (uiop:featurep :os-windows))
                          (list :application-type :gui))))))))))
  (serious-condition ()
    (uiop:quit -1)))
(uiop:quit 0)

(cl:in-package :cl-user)

;;
;; INPUT:
;;   *bodge-blob-systems*
;;   *foreign-library-dir*
;;
;; FROM build.lisp:
;;   *delivery-bundle-directory*
;;   *target-features*
;;
(when *bodge-blob-systems*
  (asdf:load-system :bodge-blobs-support))

(when *bodge-blob-systems*
  (let ((dst-dir (dir *delivery-bundle-directory* *foreign-library-dir*))
        (libraries (loop for system in *bodge-blob-systems*
                         append (bodge-blobs-support:find-system-libraries-by-features
                                 system
                                 *target-features*))))
    (ensure-directories-exist dst-dir)
    (shout "Copying foreing libraries into ~A." dst-dir)
    (loop for lib in libraries
          for path = (file (bodge-blobs-support:library-descriptor-search-path lib)
                           (bodge-blobs-support:library-descriptor-name lib))
          do (shout "Copying ~A." path)
             (cp dst-dir path))))

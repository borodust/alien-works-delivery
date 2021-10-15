(cl:in-package :cl-user)

;;
;; INPUT:
;;   *bodge-blob-systems*
;;   *foreign-library-dir*
;;
;; FROM build.lis:
;;   *delivery-bundle-directory*
;;
(apply #'asdf:load-systems *bodge-blob-systems*)

(let ((dst-dir (dir *delivery-bundle-directory* *foreign-library-dir*)))
  (ensure-directories-exist dst-dir)
  (shout "Copying foreing libraries into ~A." dst-dir)
  (loop for path in (bodge-blobs-support:list-registered-libraries)
        do (shout "Copying ~A." path)
           (cp dst-dir path)))

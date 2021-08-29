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

(loop with dst-dir = (dir *delivery-bundle-directory* *foreign-library-dir*)
      for path in (bodge-blobs-support:list-registered-libraries)
      do (shell "cp" "-L" path dst-dir))

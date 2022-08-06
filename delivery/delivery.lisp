(cl:in-package :alien-works-delivery)


(defun merge-resource-pathname (relative)
  (asdf:system-relative-pathname :alien-works-delivery relative))


(defun print-parameters (stream &rest params &key &allow-other-keys)
  (let ((*package* (find-package :alien-works-delivery~pristine)))
    (loop for (param value) on params by #'cddr
          do (prin1 `(defparameter ,(alexandria:format-symbol :cl-user "*~A*" param) ',value) stream))))


(defun extract-dependency-name (dependency-designator)
  (if (listp dependency-designator)
      (ecase (first dependency-designator)
        (:version (extract-dependency-name (second dependency-designator)))
        (:feature (extract-dependency-name (third dependency-designator)))
        (:require (values (extract-dependency-name (second dependency-designator)) t)))
      dependency-designator))


(defun collect-dependencies (system)
  (let ((dependency-table (make-hash-table :test 'equal)))
    (labels ((%collect-dependencies (system)
               (when system
                 (loop for dep-descriptor in (append (asdf:system-depends-on system)
                                                     (asdf:system-weakly-depends-on system)
                                                     (asdf:system-defsystem-depends-on system))
                       for (dep-name require-p) = (multiple-value-list (extract-dependency-name dep-descriptor))
                       for dep = (unless require-p
                                   (asdf:find-system dep-name t))
                       when dep
                         do (let ((proper-dep-name (asdf:component-name dep)))
                              (when (not (gethash proper-dep-name dependency-table))
                                (setf (gethash proper-dep-name dependency-table) dep)
                                (%collect-dependencies dep)))))))
      (%collect-dependencies system))
    (hash-table-values dependency-table)))


(defun collect-root-paths (registry-table)
  (remove-duplicates (mapcar #'asdf:system-source-directory
                             (registry-root-systems registry-table))
                     :test #'equal))


(defun collect-relative-paths (registry-table)
  (remove-duplicates (mapcar #'car (registry-relative-systems registry-table))
                     :test #'equal))


(defun copy-directory (source destination)
  (let* ((source (dir source))
         (destination (dir destination)))
    (distignore:with-ignorable-directory (source)
      (ensure-directories-exist destination)
      (when (uiop:directory-exists-p source)
        (when-let (files (remove-if #'distignore:pathname-ignored-p (uiop:directory-files source)))
          (unless (apply #'cp destination files)
            (error "Failed to copy files into directory ~A" destination)))
        (loop for dir in (remove-if #'distignore:pathname-ignored-p (uiop:subdirectories source))
              do (copy-directory dir (dir destination (first (last (pathname-directory dir))))))))))


(defun make-asdf-registry (base-system-name target-directory &key (if-exists :error))
  (let* ((base-system (asdf:find-system base-system-name))
         (systems (list* base-system (collect-dependencies base-system)))
         (registry-table (make-registry-table systems))
         (target-directory (dir target-directory)))
    (when (uiop:directory-exists-p target-directory)
      (case if-exists
        (:supersede (rm target-directory))
        (:error (error "Directory exist: ~A" target-directory))))

    (ensure-directories-exist target-directory)
    (loop for path in (collect-root-paths registry-table)
          for target-registry-directory = (dir target-directory
                                               (first (last (pathname-directory path))))
          for target-exists = (uiop:directory-exists-p target-registry-directory)
          when (and target-exists (eq if-exists :error))
            do (error "Target directory exists: ~A" target-registry-directory)
          when (or (eq if-exists :overwrite)
                   (not (uiop:directory-exists-p target-registry-directory)))
            do (copy-directory path target-registry-directory))

    (with-output-to-file (out (file target-directory "registry.lisp") :if-exists :supersede)
      (print-parameters out :registry-paths (collect-relative-paths registry-table))
      (append-file out (merge-resource-pathname "delivery/scripts/registry.lisp")))
    systems))


(defun make-builder (bundle base-system-name runner-symbol target-path)
  (with-output-to-file (out target-path :if-exists :supersede)
    (print-parameters out
                      :runner-symbol (if (and (symbolp runner-symbol)
                                              (not (keywordp runner-symbol)))
                                         (list (make-keyword (package-name
                                                              (symbol-package runner-symbol)))
                                               (make-keyword (symbol-name runner-symbol)))
                                         runner-symbol)
                      :base-system-name base-system-name
                      :delivery-bundle-features (delivery-bundle-build-features bundle))
    (append-file out (merge-resource-pathname "delivery/scripts/builder.lisp"))))


(defun make-bundler (bundle bundler-path)
  (with-output-to-file (out bundler-path :if-exists :supersede)
    (apply #'print-parameters out (delivery-bundle-assembler-parameters bundle))
    (write-delivery-bundle-assembler-source bundle out)))


(defun make-bodge-blob-collector (systems collector-path blob-dir)
  (let ((blob-systems (loop for system in systems
                            when (bodge-blobs-support:bodge-blob-system-p system)
                              collect (asdf:component-name system))))
    (with-output-to-file (out collector-path :if-exists :supersede)
      (when blob-systems
        (print-parameters out
                          :bodge-blob-systems blob-systems
                          :foreign-library-dir blob-dir)
        (append-file out (merge-resource-pathname "delivery/scripts/blobs.lisp"))))))


(defun append-file (stream file &key element-type)
  (alexandria:with-input-from-file (in file :element-type element-type)
    (uiop:copy-stream-to-stream in stream :element-type element-type)))


(defun copy-assets (assets base-dir)
  (flet ((%copy-asset (destination source)
           (let ((destination (merge-pathnames destination base-dir)))
             (ensure-directories-exist destination)
             (cp destination source))))
    (loop for asset in assets
          when (typep asset 'bundle-file)
            do (%copy-asset (bundle-file-destination asset) (bundle-file-source asset)))))


(declaim (special *delivery-bundle-directory*
                  *delivery-bundle-registry*
                  *delivery-bundle-systems*))

(defgeneric make-delivery-bundle (type bundle-def))
(defgeneric prepare-delivery-bundle (bundle))
(defgeneric delivery-bundle-foreign-library-directory (bundle))
(defgeneric delivery-bundle-asset-directory (bundle))
(defgeneric delivery-bundle-executable-path (bundle))
(defgeneric delivery-bundle-build-features (bundle))
(defgeneric delivery-bundle-assembler-parameters (bundle))
(defgeneric write-delivery-bundle-assembler-source (bundle stream))


(defun prepare-bundle-commons (bundle-def delivery-bundle-dir)
  (let* ((system-name (bundle-system-name bundle-def))
         (*print-case* :downcase))
    (ensure-directories-exist delivery-bundle-dir)

    (with-output-to-file (builder-stream (file delivery-bundle-dir "deliver.lisp")
                                         :if-exists :supersede)
      (append-file builder-stream
                   (asdf:system-relative-pathname :alien-works-delivery/util
                                                  "util/uti.lisp"))
      (append-file builder-stream
                   (merge-resource-pathname "delivery/scripts/deliver.lisp")))

    (make-asdf-registry system-name
                        (merge-pathnames "registry/"
                                         delivery-bundle-dir))))

(defun prepare-bundle (bundle system-name entry-point assets
                       systems delivery-bundle-dir)
  (let ((*print-case* :downcase))
    (ensure-directories-exist *delivery-bundle-directory*)

    (prepare-delivery-bundle bundle)

    (make-bodge-blob-collector systems
                               (file delivery-bundle-dir "blobs.lisp")
                               (delivery-bundle-foreign-library-directory bundle))

    (copy-assets assets
                 (dir *delivery-bundle-directory*
                      (delivery-bundle-asset-directory bundle)))

    (make-builder bundle
                  system-name
                  entry-point
                  (file delivery-bundle-dir "builder.lisp"))

    (make-bundler bundle (file delivery-bundle-dir "bundler.lisp"))

    (with-output-to-file (builder-stream (file delivery-bundle-dir "build.lisp")
                                         :if-exists :supersede)
      (print-parameters builder-stream
                        :bundle-executable-path (delivery-bundle-executable-path bundle))
      (append-file builder-stream
                   (merge-resource-pathname "delivery/scripts/build.lisp")))))


(defun write-bundle (bundle-path bundle-source-dir)
  (compress bundle-path (dir bundle-source-dir "delivery-bundle/")))


(defun assemble-delivery-bundle (bundle-name target-path &rest types)
  (let ((bundle-def (find-bundle-definition bundle-name)))
    (with-temporary-directory (:pathname tmp-delivery-bundle-dir)
      (let ((delivery-bundle-dir (dir tmp-delivery-bundle-dir "delivery-bundle/")))
        (multiple-value-bind (systems)
            (prepare-bundle-commons bundle-def delivery-bundle-dir)
          (loop for bundle-type in types
                do (let* ((type-str (ppcre:regex-replace-all
                                     "[\\/]"
                                     (format nil "~(~A~)" bundle-type)
                                     "-"))
                          (bundle-dir (dir delivery-bundle-dir "bundles/" type-str))
                          (*delivery-bundle-directory* bundle-dir)
                          (bundle (make-delivery-bundle bundle-type bundle-def)))
                     (prepare-bundle bundle
                                     (bundle-system-name bundle-def)
                                     (bundle-entry-point bundle-def)
                                     (bundle-assets bundle-def)
                                     systems
                                     bundle-dir)))))
      (write-bundle target-path tmp-delivery-bundle-dir))))

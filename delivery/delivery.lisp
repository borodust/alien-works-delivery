(cl:in-package :alien-works-delivery)


(defun merge-resource-pathname (relative)
  (asdf:system-relative-pathname :alien-works-delivery relative))


(defun cp (destination &rest sources)
  (apply #'shell "/bin/cp" "-LR"
         (append
          sources
          (list destination))))


(defun print-parameters (stream &rest params &key &allow-other-keys)
  (let ((*package* (find-package :alien-works-delivery~pristine)))
    (loop for (param value) on params by #'cddr
          do (prin1 `(defparameter ,(alexandria:format-symbol :cl-user "*~A*" param) ',value) stream))))


(defun extract-dependency-name (dependency-designator)
  (if (listp dependency-designator)
      (extract-dependency-name (ecase (first dependency-designator)
                                 (:version (second dependency-designator))
                                 (:feature (third dependency-designator))
                                 (:require (second dependency-designator))))
      dependency-designator))


(defun collect-dependencies (system)
  (let ((dependency-table (make-hash-table :test 'equal)))
    (labels ((%collect-dependencies (system)
               (when system
                 (loop for dep-name in (append (asdf:system-depends-on system)
                                               (asdf:system-weakly-depends-on system)
                                               (asdf:system-defsystem-depends-on system))
                       for dep = (asdf:find-system (extract-dependency-name dep-name) nil)
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


(defun read-distignore-predicate (path)
  (when-let ((distignore-file (probe-file (file path ".distignore"))))
    (labels ((trim-string (string)
               (let ((string (if-let (pos (position #\# string))
                               (subseq string 0 pos)
                               string)))
                 (string-trim '(#\Tab #\Space #\Newline) string))))
      (let* ((regexes (split-sequence:split-sequence #\Newline
                                                     (read-file-into-string distignore-file)))
             (scanners (mapcar #'ppcre:create-scanner (remove-if #'emptyp
                                                                 (mapcar #'trim-string regexes)))))
        (lambda (string)
          (let ((path (ensure-unix-namestring path))
                (string (ensure-unix-namestring string)))
            (when (starts-with-subseq path string)
              (let ((subpath (string+ "/" (enough-namestring string path))))
                (loop for scanner in scanners
                        thereis (ppcre:scan scanner subpath))))))))))


(defvar *exclusion-predicates* nil)


(defun path-excluded-p (path)
  (loop for pred in *exclusion-predicates*
          thereis (funcall pred path)))


(defun copy-directory (source destination)
  (let* ((source (dir source))
         (destination (dir destination))
         (*exclusion-predicates* (append (when-let ((pred (read-distignore-predicate source)))
                                           (list pred))
                                         *exclusion-predicates*)))
    (ensure-directories-exist destination)
    (when (uiop:directory-exists-p source)
      (when-let (files (remove-if #'path-excluded-p (uiop:directory-files source)))
        (unless (apply #'cp destination files)
          (error "Failed to copy files into directory ~A" destination)))
      (loop for dir in (remove-if #'path-excluded-p (uiop:subdirectories source))
            do (copy-directory dir (dir destination (first (last (pathname-directory dir)))))))))


(defun make-asdf-registry (base-system-name target-directory &key (if-exists :error))
  (let* ((base-system (asdf:find-system base-system-name))
         (systems (list* base-system (collect-dependencies base-system)))
         (registry-table (make-registry-table systems))
         (target-directory (dir target-directory)))
    (when (uiop:directory-exists-p target-directory)
      (case if-exists
        (:supersede (shell "rm" "-rf" target-directory))
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


(defun make-builder (base-system-name runner-symbol target-path)
  (with-output-to-file (out target-path :if-exists :supersede)
    (print-parameters out
                      :runner-symbol (list (make-keyword (symbol-name runner-symbol))
                                           (make-keyword (package-name
                                                          (symbol-package runner-symbol))))
                      :base-system-name base-system-name)
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


(defun copy-assets (system assets base-dir)
  (labels ((%system-path (relative)
             (asdf:system-relative-pathname system (uiop:relativize-pathname-directory relative)))
           (%copy-asset (destination &rest sources)
             (let* ((sources (loop for source in sources
                                   collect (if (listp source)
                                               (if (eq (first source) :system)
                                                   (%system-path (second source))
                                                   (error "Unrecognized asset source: ~A" source))
                                               source)))
                    (destination (merge-pathnames destination base-dir
                                                  )))
               (ensure-directories-exist destination)
               (apply #'cp destination sources))))
    (loop for asset in assets
          do (apply #' %copy-asset (reverse asset)))))


(declaim (special *delivery-bundle-directory*))

(defgeneric make-delivery-bundle (type system-name &key &allow-other-keys))
(defgeneric prepare-delivery-bundle (bundle))
(defgeneric delivery-bundle-foreign-library-directory (bundle))
(defgeneric delivery-bundle-asset-directory (bundle))
(defgeneric delivery-bundle-executable-path (bundle))
(defgeneric delivery-bundle-assembler-parameters (bundle))
(defgeneric write-delivery-bundle-assembler-source (bundle stream))


(defun prepare-bundle (bundle system-name runner assets tmp-delivery-bundle-dir)
  (let* ((*print-case* :downcase)
         (*delivery-bundle-directory* (dir tmp-delivery-bundle-dir "bundle/")))
    (ensure-directories-exist tmp-delivery-bundle-dir)
    (ensure-directories-exist *delivery-bundle-directory*)

    (prepare-delivery-bundle bundle)

    (let ((systems (make-asdf-registry system-name
                                       (merge-pathnames "registry/" tmp-delivery-bundle-dir))))

      (make-bodge-blob-collector systems
                                 (file tmp-delivery-bundle-dir "blobs.lisp")
                                 (delivery-bundle-foreign-library-directory bundle)))

    (copy-assets system-name assets (dir *delivery-bundle-directory*
                                         (delivery-bundle-asset-directory bundle)))

    (make-builder system-name runner (file tmp-delivery-bundle-dir "builder.lisp"))

    (make-bundler bundle (file tmp-delivery-bundle-dir "bundler.lisp"))

    (with-output-to-file (builder-stream (file tmp-delivery-bundle-dir "build.lisp")
                                         :if-exists :supersede)
      (append-file builder-stream (asdf:system-relative-pathname :alien-works-delivery-util
                                                                 "util/uti.lisp"))
      (print-parameters builder-stream
                        :bundle-executable-path (delivery-bundle-executable-path bundle))
      (append-file builder-stream (merge-resource-pathname "delivery/scripts/build.lisp")))))


(defun write-bundle (bundle-path bundle-source-dir)
  (with-output-to-file (bundle-out bundle-path :if-exists :supersede)
    (append-file bundle-out (asdf:system-relative-pathname
                             :alien-works-delivery
                             "delivery/scripts/header.lisp"))
    (append-file bundle-out (asdf:system-relative-pathname
                             :alien-works-delivery
                             "delivery/scripts/linux-delivery-bundle-prologue.lisp")))

  (with-output-to-file (bundle-out bundle-path :if-exists :append :element-type '(unsigned-byte 8))
    (uiop:with-temporary-file (:pathname tmp-bundle-archive)
      (uiop:with-current-directory (bundle-source-dir)
        (shell "tar" "-czf" tmp-bundle-archive "delivery-bundle/"))
      (append-file bundle-out tmp-bundle-archive :element-type '(unsigned-byte 8)))))


(defun assemble-delivery-bundle (type system runner bundle-path
                                 &rest bundler-args &key assets &allow-other-keys)
  (let ((bundle (apply #'make-delivery-bundle type system bundler-args)))
    (with-temporary-directory (:pathname tmp-delivery-bundle-dir)
      (prepare-bundle bundle system runner assets
                      (dir tmp-delivery-bundle-dir "delivery-bundle/") )
      (write-bundle bundle-path tmp-delivery-bundle-dir))))

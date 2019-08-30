(defun my/list-package-json-files-in-project ()
  "Return list of all package.json files in project as (relative-path-from-project-root full-path)."
  (when (projectile-project-p)
    (cl-loop with root = (projectile-project-root)
             for display in (remove-if-not
                             (lambda (name) (string-suffix-p "package.json" name))
                             (projectile-current-project-files))
             collect (cons display (expand-file-name display root)))))

(defun my/replace-jetlang-version-in-json-data (version json-data)
  "Replace @jetlang packages version with VERSION in JSON-DATA."
  (cl-loop with prefix-string = "@jetlang"
           for dependency in (remove-if-not
                              (lambda (entry) (string-prefix-p prefix-string (symbol-name (car entry))))
                              (cdr (assoc 'dependencies json-data)))
           do (setf (cdr dependency) version)
           collect dependency
           finally return json-data))

(defun my/replace-jetlang-version-in-json-file (version file)
  "Replace jetlang version with VERSION in FILE."
  (with-temp-file file
    (insert-file-contents file)
    (atomic-change-group
      (let ((json-encoding-pretty-print t)
            ;; Distinguish an empty object from 'null'
            (json-null :json-null)
            ;; Ensure that ordering is maintained
            (json-object-type 'alist)
            (txt (delete-and-extract-region (point-min) (point-max))))
        (insert (json-encode
                 (my/replace-jetlang-version-in-json-data version (json-read-from-string txt))))
        ;; Add trailing newline at the end of file
        (insert "\n")))))

(defun my/replace-all-package-json-in-project-with-version (version)
  "Replace jetlang version with VERSION in all package.json files in current project."
  (interactive "sVersion:")
  (cl-loop for file in (my/list-package-json-files-in-project)
           do (my/replace-jetlang-version-in-json-file version (cdr file))))

(provide 'init-payfit)
;;; init-payfit.el ends here

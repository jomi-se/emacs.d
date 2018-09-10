(maybe-require-package 'json-mode)
(maybe-require-package 'js2-mode)
(maybe-require-package 'coffee-mode)
(maybe-require-package 'typescript-mode)
(maybe-require-package 'prettier-js)
(maybe-require-package 'rjsx-mode)
(maybe-require-package 'add-node-modules-path)


(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
;; This is the equivalent of doing something like:
;; redefine A = New-list-entry + (A - js-modes)
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))

(require 'flycheck)
;; js2-mode

;; Change some defaults: customize them to override
(setq js2-idle-timer-delay 1)
(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(defun my/use-eslint-from-node-modules ()
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
                   (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
        (setq-local flycheck-javascript-eslint-executable eslint)))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(require 'js2-mode)

;; Disable js2 mode's syntax error highlighting by default...
(setq-default js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil)
(defun setup-js2-mode ()
  (flycheck-mode)
  (my/use-eslint-from-node-modules))
(add-hook 'js2-mode-hook 'setup-js2-mode)
;; ... but enable it if flycheck can't handle javascript
;; (autoload 'flycheck-get-checker-for-buffer "flycheck")
;; (defun sanityinc/disable-js2-checks-if-flycheck-active ()
;;   (unless (flycheck-get-checker-for-buffer)
;;     (set (make-local-variable 'js2-mode-show-parse-errors) t)
;;     (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
;; (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)

(add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

(after-load 'js2-mode
  (js2-imenu-extras-setup))

;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)

(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))



(when (and (executable-find "ag")
           (maybe-require-package 'xref-js2))
  (after-load 'js2-mode
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook
              (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))



;;; Coffeescript

(after-load 'coffee-mode
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'js-comint)
  (setq inferior-js-program-command "node")

  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
  (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
  (define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
  (define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    nil " InfJS" inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode)))

;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'skewer-mode)
  (after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))

;; ---------------------------------------------------------------------------
;; flyspell setup for js2-mode (taken from bin chen)
;; ---------------------------------------------------------------------------

(defun js-flyspell-verify ()
  (let* ((f (get-text-property (- (point) 1) 'face)))
    ;; *whitelist*
    ;; only words with following font face will be checked
    (memq f '(js2-function-call
              js2-function-param
              js2-object-property
              font-lock-variable-name-face
              font-lock-string-face
              font-lock-function-name-face
              font-lock-builtin-face
              rjsx-tag
              rjsx-attr))))
(put 'js2-mode 'flyspell-mode-predicate 'js-flyspell-verify)
;; }}


;; Tern
(require-package 'tern)
(require-package 'company-tern)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))



(after-load 'company
  (add-hook 'js2-mode-hook
            (lambda () (sanityinc/local-push-company-backend 'company-tern))))


;; Prettier
(defun setup-prettier ()
  (setq prettier-js-args '("--write" ""))
  (prettier-js-mode t))
;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook #'add-node-modules-path))
;; (after-load 'js2-mode
;;   (add-hook 'js2-mode-hook 'setup-prettier))


;; (when (maybe-require-package 'add-node-modules-path)
;;   (after-load 'typescript-mode
;;     (add-hook 'typescript-mode-hook 'add-node-modules-path))
;;   (after-load 'js2-mode
;;     (add-hook 'js2-mode-hook 'add-node-modules-path)))


(provide 'init-javascript)

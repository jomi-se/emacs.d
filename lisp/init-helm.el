(require-package 'helm)
(require-package 'helm-config)
(require-package 'helm-projectile)
(require-package 'helm-xref)
(require-package 'helm-gtags)

(if (executable-find "ag")
    (require-package 'helm-ag)
  (message "Executable for ag was not found"))

(maybe-require-package 'flyspell-correct-helm)

;;  Taken from https://tuhdo.github.io/helm-intro.html

;; Key Binding  Command                 Description
;; M-x          helm-M-x                List commands
;; M-y          helm-show-kill-ring     Shows the content of the kill ring
;; C-x b        helm-mini               Shows open buffers, recently opened files
;; C-x C-f      helm-find-files         The helm version of find-file
;; C-s          helm-ff-run-grep        Run grep from within helm-find-files
;; C-c h i      helm-semantic-or-imenu  Helm interface to semantic/imenu
;; C-c h m      helm-man-woman          Jump to any man entry
;; C-c h /      helm-find               Helm interface to find
;; C-c h l      helm-locate             Helm interface to locate
;; C-c h o      helm-occur              Helm interface for occur
;; C-c h a      helm-apropos            Describes commands, functions, variables, …
;; C-c h h g    helm-info-gnus
;; C-c h h i    helm-info-at-point
;; C-c h h r    helm-info-emacs
;; C-c h b      helm-resume              Resumes a previous helm session
;; C-h SPC      helm-all-mark-rings      Views contents of local and global mark rings
;; C-c h r      helm-regex               Visualizes regex matches
;; C-c h x      helm-register            Shows content of registers
;; C-c h t      helm-top                 Helm interface to top
;; C-c h s      helm-surfraw             Command line interface to many web search engines
;; C-c h g      helm-google-suggest      Interactively enter search terms and get results from Google in helm buffer
;; C-c h c      helm-color               Lists all available faces
;; C-c h C-,    helm-calcul-expression   Helm interface to calc
;; C-c C-l      helm-eshell-history      Interface to eshell history
;; C-c C-l      helm-comint-input-ring   Interface to shell history
;; C-c C-l      helm-mini-buffer-history Interface to mini-buffer history

;; C-c h M-:    helm-eval-expression-with-eldoc Get instant results for Emacs lisp expressions in the helm buffer
;; C-c h <tab>  helm-lisp-completion-at-point   Provides a list of available functions

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(bind-key* (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(bind-keys :map helm-map
           ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
           ("C-i" . helm-execute-persistent-action) ; make TAB work in terminal
           ("C-z" .  helm-select-action)) ; list actions using C-z

(bind-key* (kbd "M-y") 'helm-show-kill-ring)
(bind-key* (kbd "M-x") 'helm-M-x)
(bind-key* (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h <SPC>") 'helm-all-mark-rings)
(bind-key* (kbd "C-x b") 'helm-mini)
(bind-key* (kbd "C-x C-f") 'helm-find-files)

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(setq helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-M-x-fuzzy-match                  t
      helm-split-window-in-side-p           nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line 	    t)

;; Use this to toggle opening helm buffer inside current window, not occupy whole other window
(defun my-toggle-helm-use-whole-frame-when-in-split-window ()
  "Toggle helm-split-window-in-side-p between t and nil"
  (interactive)
  (setq helm-split-window-in-side-p (not helm-split-window-in-side-p)))

(with-eval-after-load 'helm-semantic
      (push '(c-mode . semantic-format-tag-summarize) helm-semantic-display-style)
      (push '(c++-mode . semantic-format-tag-summarize) helm-semantic-display-style)
      (push '(js2-mode . semantic-format-tag-summarize) helm-semantic-display-style))

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 40)
(helm-autoresize-mode 1)

(setq
 helm-gtags-ignore-case t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-suggested-key-mapping t
 )

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

(bind-keys :map helm-gtags-mode-map
           ("C-c g a" . helm-gtags-tags-in-this-function)
           ("M-." . helm-gtags-dwim)
           ("M-," . helm-gtags-pop-stack)
           ("C-c <" . helm-gtags-previous-history)
           ("C-c >" . helm-gtags-next-history))

(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile-find-file)
(helm-projectile-on)

(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

(helm-mode 1)

(provide 'init-helm)

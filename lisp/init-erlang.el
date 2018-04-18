(when (maybe-require-package 'erlang)
  (require 'erlang-start))

;; ;; Taken from http://www.lambdacat.com/post-modern-emacs-setup-for-erlang/
;; 
;; ;;Erlang-mode from current build
;; (setq load-path (cons "//path/to//Erlang_installs/18.3/lib/tools-2.8.3/emacs" load-path))
;; (setq erlang-root-dir "//path/to/Erlang_installs/18.3/")
;; (setq exec-path (cons "//path/to/Erlang_installs/18.3/bin" exec-path))
;; (setq erlang-man-root-dir "/home/path/to/Erlang_installs/18.3/man")
;; (require 'erlang-start)
;; 
;; ;; fly-check for Erlang
;; (require 'flycheck)
;; (flycheck-define-checker erlang-otp
;;                          "An Erlang syntax checker using the Erlang interpreter."
;;                          :command ("erlc" "-o" temporary-directory "-Wall"
;;                                    "-I" "../include" "-I" "../../include"
;;                                    "-I" "../../../include" source)
;;                          :modes erlang-mode
;;                          :error-patterns
;;                          ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
;;                           (error line-start (file-name) ":" line ": " (message) line-end)))
;; 
;; (add-hook 'erlang-mode-hook
;;           (lambda ()
;;             (flycheck-select-checker 'erlang-otp)
;;             (flycheck-mode)))
;; 
;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))
;; 
;; ;; Distel setup
;; (add-to-list 'load-path "~/.emacs.d/distel/elisp")
;; (require 'distel)
;; (distel-setup)
;; 
;; (defun my-erlang-mode-hook ()
;;         ;; when starting an Erlang shell in Emacs, default in the node name
;;         (setq inferior-erlang-machine-options '("-sname" "emacs"))
;;         ;; add Erlang functions to an imenu menu
;;         (imenu-add-to-menubar "imenu"))
;; 
;; ;; Some Erlang customizations
;; ;; Auto-complete
;; (add-hook 'erlang-mode-hook 'company-mode)
;; (add-hook 'erlang-mode-hook
;;           (lambda ()
;;             (setq company-backends '(company-distel))))
;; (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
;; (add-hook 'erlang-mode-hook
;; 	  (lambda () (local-set-key (kbd "<C-tab>") 'company-complete)))
;; 
;; ;; Erlang Shell autocomplete
;; (add-hook 'erlang-shell-mode-hook 'company-mode)
;; (add-hook 'erlang-shell-mode-hook
;;           (lambda ()
;;             (setq company-backends '(company-distel))))
;; 
;; ;; Erlang Shell custom keys
;; (add-hook 'erlang-shell-mode-hook
;; 	  (lambda () (local-set-key (kbd "<C-tab>") 'company-complete)))
;; (add-hook 'erlang-shell-mode-hook
;; 	  (lambda () (local-set-key (kbd "C-c g") 'erl-find-source-under-point)))
;; (add-hook 'erlang-shell-mode-hook
;; 	  (lambda () (local-set-key (kbd "C-c t") 'erl-find-source-unwind)))
;; 
;; ;; Make Erlang Shell prompt and pas output read-only:
;; ;; (this method is valid for all comint-derived modes)
;; 
;; (add-hook 'erlang-shell-mode-hook
;;           (lambda ()
;;             (setq comint-prompt-read-only t)))
;; 
;; (defun my-comint-preoutput-turn-buffer-read-only (text)
;;   (propertize text 'read-only t))
;; 
;; (add-hook 'erlang-shell-mode-hook
;;           (lambda ()
;;             (add-hook 'comint-preoutput-filter-functions
;;                       'my-comint-preoutput-turn-buffer-read-only)))
;; 

(provide 'init-erlang)

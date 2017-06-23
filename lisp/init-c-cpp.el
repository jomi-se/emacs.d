;; C PROGRAMMING HOOKS
;; Set linux kernel C programming style
(setq c-default-style "linux")
(setq-default indent-tabs-mode nil)
(add-hook 'c-mode-hook
          (lambda () (setq c-basic-offset 4)))

;; Always turn on hi-lock (highlighting) minor mode in C
(add-hook 'c-mode-hook 'hi-lock-mode)

;; set dabbrev-expand to C-TAB for C-mode
(add-hook 'c-mode-hook
	  (lambda () (local-set-key (kbd "<C-tab>") 'hippie-expand)))

;; set S-TAB for indent-relative in C-mode
(add-hook 'c-mode-hook
	  (lambda () (local-set-key (kbd "<backtab>") 'indent-relative)))

;; set C-x C-a to ff-find-other-file to easily switch between header and source
(add-hook 'c-mode-hook
	  (lambda () (local-set-key (kbd "C-x C-a") 'ff-find-other-file)))

;; Activate semantic mode
(require 'semantic/sb)
(semantic-mode 1)
(require 'ede)
(global-ede-mode)
;; Add the following to a .dir-locals.el at the root of a C/C++ project for completion stuff
;; (eval ede-cpp-root-project "Test" :name "Test Project" :file "~/Path/to/file/in/project/root" :include-path
;;       (quote
;;        ("/"))
;;       :system-include-path
;;       (quote
;;        ("~/path/to/project/include1" "~/path/to/project/include2")))

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-decoration-mode 0)
(global-semantic-idle-summary-mode 1)
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

(add-hook 'c-mode-hook
	  (lambda () (sanityinc/local-push-company-backend 'company-semantic)))

(provide 'init-c-cpp)

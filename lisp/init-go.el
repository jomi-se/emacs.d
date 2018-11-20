(require-package 'go-mode)

(add-hook 'before-save-hook 'gofmt-before-save)

(after-load 'company
  (add-hook 'go-mode-hook (lambda () (sanityinc/local-push-company-backend 'company-go))))

(require 'flycheck)

(defun go-mode-setup ()
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (setq gofmt-command "goimports"))
(add-hook 'go-mode-hook 'go-mode-setup)

(provide 'init-go)
;;; init-go.el ends here

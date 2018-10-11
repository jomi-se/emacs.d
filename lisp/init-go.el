(require-package 'go-mode)

(add-hook 'before-save-hook 'gofmt-before-save)

(after-load 'company
  (add-hook 'go-mode-hook (lambda () (sanityinc/local-push-company-backend 'company-go))))

(require 'flycheck)

(provide 'init-go)
;;; init-go.el ends here

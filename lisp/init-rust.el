(when (maybe-require-package 'rust-mode)
  (when (maybe-require-package 'racer)
    (maybe-require-package 'rust-snippets)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
    (add-hook 'racer-mode-hook #'eldoc-mode))
  (when (maybe-require-package 'company)
    (add-hook 'racer-mode-hook #'company-mode)))

(when (maybe-require-package 'flycheck-rust)
  (after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'init-rust)

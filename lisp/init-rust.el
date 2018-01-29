(when (maybe-require-package 'rust-mode)
  (when (and (maybe-require-package 'racer)
             (maybe-require-package 'flycheck-rust)
             (maybe-require-package 'company-racer))
    (maybe-require-package 'rust-snippets)
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
    (add-hook 'rust-mode-hook #'flycheck-rust-setup)))

(provide 'init-rust)

(when (maybe-require-package 'yaml-mode)
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode)
  (add-hook 'yaml-mode-hook 'indent-guide-mode))


(provide 'init-yaml)

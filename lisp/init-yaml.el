(when (maybe-require-package 'yaml-mode)
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode)
  (add-hook 'yaml-mode-hook 'indent-guide-mode)
  (flycheck-add-mode 'yaml-yamllint 'yaml-mode)
  (flycheck-add-next-checker 'yaml-ruby '(warning . yaml-yamllint) 'append))


(provide 'init-yaml)

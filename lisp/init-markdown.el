(when (maybe-require-package 'markdown-mode)
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))
  (after-load 'markdown-mode
    (add-hook 'markdown-mode-hook #'visual-line-mode)))


(provide 'init-markdown)

(when (maybe-require-package 'buffer-move)
  ;; Buffer move keybindings
  (bind-key* (kbd "<C-S-up>")     'buf-move-up)
  (bind-key* (kbd "<C-S-down>")   'buf-move-down)
  (bind-key* (kbd "<C-S-left>")   'buf-move-left)
  (bind-key* (kbd "<C-S-right>")  'buf-move-right))

(provide 'init-buffer-move)

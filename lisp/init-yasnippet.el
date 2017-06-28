(unless (file-exists-p (expand-file-name "yasnippet-snippets" user-emacs-directory))
  (github-clone "https://github.com/AndreaCrotti/yasnippet-snippets.git" user-emacs-directory))

(require-package 'yasnippet)
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
(yas-global-mode 1)

(provide 'init-yasnippet)

(require 'init-git)

(maybe-require-package 'yagist)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(maybe-require-package 'github-clone)
(when (maybe-require-package 'magithub)
  (magithub-feature-autoinject t))


(provide 'init-github)

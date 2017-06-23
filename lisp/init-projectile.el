(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; The following code means you get a menu if you hit "C-c p" and wait
  (after-load 'guide-key
    (add-to-list 'guide-key/guide-key-sequence "C-c p"))

  (setq projectile-enable-caching t
        projectile-indexing-method 'alien)

  (add-to-list 'projectile-globally-ignored-file-suffixes ".o")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".d")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".Plo")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".Po")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".lo")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".so")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".la")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".pyc")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".min.js")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".gz")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".jpg")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".zip")

  ;; Shorter modeline
  (after-load 'projectile
    (setq-default
     projectile-mode-line
     '(:eval
       (if (file-remote-p default-directory)
           " Proj"
         (format " Proj[%s]" (projectile-project-name)))))))


(provide 'init-projectile)

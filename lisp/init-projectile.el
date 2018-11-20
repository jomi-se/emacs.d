(when (maybe-require-package 'projectile)

  ;; The following code means you get a menu if you hit "C-c p" and wait
  (after-load 'guide-key
    (add-to-list 'guide-key/guide-key-sequence "C-c p"))


  ;; Shorter modeline
  (after-load 'projectile
    (setq-default
     projectile-mode-line
     '(:eval
       (if (file-remote-p default-directory)
           " Pr"
         (format " Pr[%s]" (projectile-project-name)))))))

(projectile-mode 1)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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


(provide 'init-projectile)

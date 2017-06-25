(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

;;------------------------------------------------------------------------------
;; Download some personal color themes and put them in
;;------------------------------------------------------------------------------
;; TODO: This should probably be done in a more generic way, but I
;;       don't know how to do that in elisp
(defvar perso-color-themes-path (expand-file-name "extra-color-themes" user-emacs-directory))

(defun download-perso-color-themes ()
  "Download some color themes I like"
  (unless (file-exists-p (expand-file-name "hc-zenburn-emacs" perso-color-themes-path))
    (github-clone "jomi-se/hc-zenburn-emacs" perso-color-themes-path))
  (add-to-list 'custom-theme-load-path (expand-file-name "hc-zenburn-emacs" perso-color-themes-path))
  (unless (file-exists-p (expand-file-name "blackboard-theme" perso-color-themes-path))
    (github-clone "don9z/blackboard-theme" perso-color-themes-path))
  (add-to-list 'custom-theme-load-path (expand-file-name "blackboard-theme" perso-color-themes-path)))

(download-perso-color-themes)

(provide 'init-themes)

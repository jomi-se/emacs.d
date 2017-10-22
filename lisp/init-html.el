(require-package 'tidy)
(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

(require-package 'tagedit)
(after-load 'sgml-mode
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1)))
  (add-hook 'sgml-mode-hook
            (lambda ()
              ;; Default indentation to 2, but let SGML mode guess, too.
              (set (make-local-variable 'sgml-basic-offset) 2)
              (sgml-guess-indent)))
  (add-hook
   'sgml-mode-hook
   (lambda ()
     (setq-local electric-pair-inhibit-predicate
                 `(lambda (c)
                    (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

;; Note: ERB is configured in init-ruby-mode

(provide 'init-html)

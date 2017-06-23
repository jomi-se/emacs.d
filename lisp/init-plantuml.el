(when (maybe-require-package 'plantuml-mode)
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

  (add-hook 'plantuml-mode-hook
            (lambda () (local-set-key (kbd "<C-tab>") 'plantuml-complete-symbol))))

(provide init-plantuml)

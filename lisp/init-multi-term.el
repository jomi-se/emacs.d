(require-package 'multi-term)

(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("M-<prior>" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-<next>" . multi-term-next))
            (set-face-attribute 'default (selected-frame) :background "#000000") ;;TODO: There is probably a cleaner way to do this
            (face-remap-add-relative 'default :background "#000000")
            (setq show-trailing-whitespace nil)))

(provide 'init-multi-term)

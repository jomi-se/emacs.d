;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(require 'ispell)

;; Variable names taken from spacemacs
(defvar spell-checking-enable-by-default t
  "Enable spell checking by default.")

(defvar spell-checking-enable-auto-dictionary t
  "Specify if auto-dictionary should be enabled or not.")

(defvar enable-flyspell-auto-completion nil
  "If not nil, show speeling suggestions in popups.")

(defun init-flyspell-config ()
  "Set up all spell check stuff"
  (when (and (maybe-require-package 'auto-dictionary)
             (maybe-require-package 'flyspell)
             (maybe-require-package 'flyspell-correct)))

  (when 'enable-flyspell-auto-completion
    (maybe-require-package 'flyspell-popup)
    (define-key flyspell-mode-map (kbd "C-!") #'flyspell-popup-correct))

  (after-load 'flyspell
    '((progn )
     (maybe-require-package 'flyspell-lazy)
     (flyspell-lazy-mode 1)))
  (setq ispell-program-name "aspell")
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)

  (define-key flyspell-mode-map (kbd "C-c s n") 'my-flyspell-goto-next-error-repeatable)
  (define-key flyspell-mode-map (kbd "C-c s p") 'my-flyspell-goto-previous-error-repeatable)
  (define-key flyspell-mode-map (kbd "C-$") 'flyspell-auto-correct-word)
  (define-key flyspell-mode-map (kbd "C-!") 'flyspell-correct-previous-word-generic)
  (define-key flyspell-mode-map (kbd "C-c s b") 'flyspell-buffer)

  (when 'spell-checking-enable-auto-dictionary
    (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))))

(if (executable-find "aspell")
    (init-flyspell-config)
  (message "Aspell not found, disabling spell checking stuff"))

;; Move point to previous error.
;; Based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; goto beginning of buffer
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; save the current location for next invocation
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
            (progn
              (message "No more miss-spelled word!")
              (setq arg 0))
          (forward-word)))))

(defun my-flyspell-goto-next-error-repeatable ()
    "Call call-flyspell-goto-next-error and repeat"
    (interactive)
    (flyspell-goto-next-error)
    (let ((repeat-key last-command-event))
      (unless (current-message)
        (message "(Type %s to repeat goto next error)" (format-kbd-macro (vector repeat-key) nil)))
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map (vector repeat-key)
           `my-flyspell-goto-next-error-repeatable)
         map))))

(defun my-flyspell-goto-previous-error-repeatable ()
    "Call call-flyspell-goto-next-error and repeat"
    (interactive)
    (flyspell-goto-previous-error 1)
    (backward-word)
    (let ((repeat-key last-command-event))
      (unless (current-message)
        (message "(Type %s to repeat goto next error)" (format-kbd-macro (vector repeat-key) nil)))
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map (vector repeat-key)
           `my-flyspell-goto-previous-error-repeatable)
         map))))

(defun my-change-dictionary ()
    "Call the correct change dictionary version depending on whether auto-dictionary is enabled or not"
    (if 'spell-checking-enable-auto-dictionary
        (adict-change-dictionary)
      (call-interactively 'ispell-change-dictionary)))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))

(provide 'init-flyspell)

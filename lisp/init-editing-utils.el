(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))

(maybe-require-package 'list-unicode-display)

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search nil
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines t
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

(add-to-list 'minor-mode-alist '(case-fold-search " CFS"))
;; Toggle CFS func
(defun toggle-case () (interactive) (setq case-fold-search (not case-fold-search)))

;; file backups config
(setq backup-directory-alist '(("." . "~/.backups_emacs"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; file autosave config
(setq auto-save-default t)
(defconst autosaves-dir (expand-file-name "~/.autosaves_emacs/"))
(setq auto-save-file-name-transforms `((".*" ,autosaves-dir t)))

;; Disable locking files
(setq create-lockfiles nil)


;; Huge files

(require-package 'vlf)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))


;;; A simple visible bell which works in all terminal types
(require-package 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)



(when (maybe-require-package 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5)
  (add-hook 'after-init-hook 'beacon-mode))



;;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun jomi-se/vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun jomi-se/vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun jomi-se/vi-open-line (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (if abovep
      (jomi-se/vi-open-line-above)
    (jomi-se/vi-open-line-below)))

(global-set-key (kbd "C-<return>") 'jomi-se/vi-open-line)
(global-set-key (kbd "C-S-<return>") 'jomi-se/vi-open-line-above)



(after-load 'subword
  (diminish 'subword-mode))



(when (maybe-require-package 'indent-guide)
  (add-hook 'prog-mode-hook 'indent-guide-mode)
  (after-load 'indent-guide
    (diminish 'indent-guide-mode)))



(add-hook 'after-init-hook (lambda ()
                             (global-linum-mode t)
                             (require 'linum-off)
                             (add-to-list 'linum-disabled-modes-list 'term-mode)
                             (add-to-list 'linum-disabled-modes-list 'helm-mode)
                             (add-to-list 'linum-disabled-modes-list 'json-mode)
                             (setq linum-disable-starred-buffers t)
                             (setq linum-disable-max-file-lines 4000)))


(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))



(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))


(require-package 'undo-tree)
(add-hook 'after-init-hook 'global-undo-tree-mode)
(after-load 'undo-tree
  (diminish 'undo-tree-mode))


(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (after-load 'symbol-overlay
    (diminish 'symbol-overlay-mode)
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))

(when (maybe-require-package 'highlight-symbol)
  (setq highlight-symbol-on-navigation-p t)
  (defun jomi-se/highlight-symbol-at-point-no-toggle (&optional symbol)
    "Highlight symbol at point if it isn't already highlighted"
    (interactive)
    (let ((symbol (or symbol
                      (highlight-symbol-get-symbol)
                      (error "No symbol at point"))))
      (if (highlight-symbol-symbol-highlighted-p symbol)
          nil
        (highlight-symbol-add-symbol symbol)
        (when (member 'explicit highlight-symbol-occurrence-message)
          (highlight-symbol-count symbol t)))))

  (defun jomi-se/highlight-and-search-next ()
    "Highlight symbol under point and search for the next occurence in buffer."
    (interactive)
    (jomi-se/highlight-symbol-at-point-no-toggle)
    (highlight-symbol-next))
  (defun jomi-se/highlight-and-search-prev ()
    "Highlight symbol under point and search for the next occurence in buffer."
    (interactive)
    (jomi-se/highlight-symbol-at-point-no-toggle)
    (highlight-symbol-prev))
  (defun jomi-se/emulate-vim-search-symbol-at-point (&optional prev)
    "Highlight and search as in Vim"
    (interactive "P")
    (if prev
        (jomi-se/highlight-and-search-prev)
      (jomi-se/highlight-and-search-next)))
  (bind-key* (kbd "C-*") 'jomi-se/emulate-vim-search-symbol-at-point)
  (bind-key* (kbd "C-S-*") 'jomi-se/highlight-and-search-prev)

  (require 'highlight-symbol))

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)



(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))


;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'show-paren-mode)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
                                        ;(global-set-key (kbd "C-.") 'set-mark-command)
                                        ;(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer)
  (global-set-key (kbd "M-l") 'avy-goto-line))

(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(bind-key* "C-M-<backspace>" 'kill-back-to-indentation)


;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (after-load 'page-break-lines
    (diminish 'page-break-lines-mode)))

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(require-package 'move-dup)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c u") 'md/duplicate-up)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(require-package 'whole-line-or-region)
(add-hook 'after-init-hook 'whole-line-or-region-mode)
(after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-mode))



(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))




(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)


(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r" "M-s" "C-h" "C-c C-a"))
(add-hook 'after-init-hook 'guide-key-mode)
(after-load 'guide-key
  (diminish 'guide-key-mode))


(defun jomi-se/insert-pair-around-line (str)
  (interactive "sSurrounding text: ")
  (goto-char (line-beginning-position))
  (insert str)
  (end-of-line)
  (insert str))

(defun jomi-se/insert-pair-line-or-mark (str)
  (interactive "sSurrounding text:")
  (if (use-region-p)
      (insert-pair 1 str str)
    jomi-se/insert-pair-line-or-mark))

;; Functions to increment/decrement decimal numbers (can be prefixed with C-u <number>)
(defun jomi-se/increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun jomi-se/decrement-number-decimal (&optional arg)
  (interactive "p*")
  (jomi-se/increment-number-decimal (if arg (- arg) -1)))

;; set increment/decrement decimals func to C-c +/-
(bind-key* (kbd "C-c +") 'jomi-se/increment-number-decimal)
(bind-key* (kbd "C-c -") 'jomi-se/decrement-number-decimal)

(bind-key* (kbd "<backtab>") 'indent-relative)

(defun my/json-flatten-object-one-level (begin end)
  "(my/json-flatten-object-one-level BEGIN END) Pretty-print selected region but only one level."
  (interactive "r")
  (let ((json-object-type 'alist)
        (txt (delete-and-extract-region begin end)))
    (insert (format "{\n%s\n}"
                    (mapconcat
                     'identity
                     (mapcar
                      (lambda (cons)
                        (format
                         "%s: %s"
                         (json-encode-key (car cons))
                         (json-encode (cdr cons))))
                      (json-read-from-string txt))
                     ",\n")))))

(defun my/json-flatten-object-one-level (begin end)
  "(my/json-flatten-object-one-level BEGIN END) Pretty-print selected region but only one level."
  (interactive "r")
  (let ((json-pretty-print-max-indentation-level 1))
    (json-pretty-print begin end)))

;; Consider CamelCase chunks as words :o
(global-subword-mode 1)

(provide 'init-editing-utils)

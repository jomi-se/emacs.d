(when (maybe-require-package 'p4)
  ;; Always ask for changelist when modifying file
  (setq p4-open-in-changelist t)

  (defun p4-change-client-and-sync ()
    "Change current client workspace, sync and update so that we load current changelists and stuff"
    (interactive)
    (call-interactively 'p4-set-client-name)
    (p4-sync)
    (p4-update)))

(provide 'init-p4)

(defun my-evil-toggle ()
  (interactive)
  (cond
    ((memq evil-state '(insert normal))
     (evil-emacs-state))
    ((eq evil-state 'emacs)
     (evil-exit-emacs-state))))

(add-hook 'magit-blame-mode-hook 'my-evil-toggle)

(setq magit-revert-buffers 'silent)
(setq magit-push-always-verify nil)



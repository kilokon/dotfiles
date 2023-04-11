;;Path to Shell
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))


(use-package vterm
  :commands (vterm vterm-other-window)
  :bind (("C-c v" . vterm)
         :map vterm-mode-map
         ("C-g" . vterm--self-insert)))


(use-package vterm-toggle 
  :bind (("<f2>" . vterm-toggle)
   	 ("C-<f2>" . vterm-toggle-cd)
	 :map vterm-mode-map(
			     "<f2>" . vterm-toggle))
  :config (setq vterm-toggle-fullscreen-p nil
;;		vterm-toggle-hide-method nil
		vterm-toggle-reset-window-configration-after-exit 'kill-window-only))

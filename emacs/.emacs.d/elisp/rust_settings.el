(use-package rustic
  ;;  :ensure
  :config
  ;; (setq rustic-lsp-client 'eglot)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-kill-buffer-and-window t
	rustic-spinner-type 'progress-bar)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  :general (kilo-leader-key
	     :keymaps 'rustic-mode-map
	     "r"   '(:ignore t                 :which-key "Rust Cargo")
	     "rr"  '(rustic-cargo-run          :which-key "Cargo Run")
	     "rb"  '(rustic-cargo-build        :which-key "Cargo Build")
	     "rt"  '(rustic-cargo-current-test :which-key "Run Test at Point")
	     "rT"  '(rustic-cargo-test         :which-key "Cargo Test")
	     "rd"  '(rustic-cargo-doc          :which-key "Cargo Doc")
	     "ra"  '(rustic-cargo-add          :which-ky "Cargo Add")
	     "rm"  '(rustic-cargo-rm           :which-key "Cargo Remove")))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(remove-hook 'rustic-mode-hook 'flycheck-mode)

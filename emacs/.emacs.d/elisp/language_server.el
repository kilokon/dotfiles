;;(spinner "1.7.3")
;; (use-package spinner
  ;; :ensure t)


;; (use-package lsp-mode
;;   :commands lsp
;;   :custom
;;   (lsp-completion-provider :none) ;; we use Corfu!
;;   :init
;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(flex))) ;; Configure flex
;;   :hook
;;   (lsp-completion-mode . my/lsp-mode-setup-completion)
;;   ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   ;;   (lsp-eldoc-render-all t)
;;   ;;   (lsp-idle-delay 0.6)
;;   ;;     ;; enable / disable the hints as you prefer:
;;   ;; (lsp-rust-analyzer-server-display-inlay-hints t)
;;   ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   ;; (lsp-rust-analyzer-display-chaining-hints t)
;;   ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   ;; (lsp-rust-analyzer-display-closure-return-type-hints t)
;;   ;; (lsp-rust-analyzer-display-parameter-hints nil)
;;   ;; (lsp-rust-analyzer-display-reborrow-hints nil)
;;   ;; (lsp-diagnostics-provider :capf)
;;   ;; (lsp-headerline-breadcrumb-enable t)
;;   ;; (lsp-headerline-breadcrumb-segments '(project file symbols))
;;   ;; (lsp-lens-enable nil)
;;   ;; (lsp-disabled-clients '((python-mode . pyls)))
;;   ;; :config
;;   ;; (setq rustic-lsp-server 'rust-analyzer)
;;   ;; :general (kilo-leader-key
;; 	     ;; :keymaps 'lsp-mode-map
;; 	     ;; "ld" '(xref-find-definitions   :which-key "Find Definitions")
;; 	     ;; "lr" '(xref-find-references    :which-key "Find References")
;; 	     ;; "lq" '(lsp-workspace-restart   :which-key "Restart Lsp")
;; 	     ;; "lx" '(lsp-workspace-shutdown  :which-key "Kill Lsp Server")
;;   ;; "la" '(lsp-execute-code-action :which-key "Execute Code Action")
;;   )
(use-package lsp-mode
:custom
(lsp-completion-provider :none) ;; we use Corfu!

:init
(defun my/orderless-dispatch-flex-first (_pattern index _total)
(and (eq index 0) 'orderless-flex))

(defun my/lsp-mode-setup-completion ()
(setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
'(orderless)))

;; Optionally configure the first word as flex filtered.
(add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

;; Optionally configure the cape-capf-buster.
(setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

:hook
(lsp-completion-mode . my/lsp-mode-setup-completion))
;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :bind
;;   (:map lsp-mode-map
;;         ("C-c C-r" . lsp-ui-peek-find-references)
;;         ("C-c C-j" . lsp-ui-peek-find-definitions)
;;         ;; ("C-c m"   . lsp-ui-imenu)
;;         ("C-c s"   . lsp-ui-sideline-mode)
;;         ("C-c d"   . aviik/toggle-lsp-ui-doc))
;;   :custom
;; ;;  (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable nil)
;;   (lsp-ui-sideline-code-actions-prefix ""))

;; (setq lsp-completion-provider :none)
;; (defun corfu-lsp-setup ()
;;   (setq-local completion-styles '(orderless)
;;               completion-category-defaults nil))
;; (add-hook 'lsp-mode-hook #'corfu-lsp-setup)

;; (use-package consult-lsp
;;   :general (kilo-leader-key
;; 	    :keymaps 'lsp-mode-map
;; 	    "c"   '(:ignore t                 :which-key "Consult-Lsp")
;; 	    "cd"  '(consult-lsp-diagnostics   :which-key "Lsp Diagnostics")
;; 	    "ci"  '(consult-imenu             :which-key "Consult Imenu")
;; 	    "cf"  '(consult-flymake           :which-key "Consult Flymake")
;; 	    "cs"  '(consult-lsp-symbols       :which-key "Consult Lsp Symbols")
;; 	    ;; "cfa" '(consult-lsp-file-symbols  :which-key "Consult Lsp File Symbols")
;; 	    ))

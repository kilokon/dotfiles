:;;Aviik's Config
;;--------------


;;+-------------------------+
;;|   Early Configuration   |
;;+-------------------------+
(setq gc-cons-threshold most-positive-fixnum) ; avoid GC during startup to save time

;;PERSONAL INFO
;;-------------

;;Personal information
(setq user-full-name "aviik c"
      user-mail-address "aviik.chakraborty@gmail.com")

;;OS related
;;----------
(setq aviikc/is-windows (eq system-type 'windows-nt))

(defvar aviikc/op-sys-available '(("osx" . "darwin")
				  ("cygwin" . "cygwin")
				  ("linux" . "gnu/linux")))

(defun aviikc/reload-init-file()
 "It reloads Emacs init config."
 (interactive)
 (load-file user-init-file))

(global-set-key (kbd "C-c r") 'aviikc/reload-init-file)


(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key

;; (global-set-key (kbd "s-right") 'previous-buffer)
(define-key global-map [s-right] 'next-buffer)
(define-key global-map [s-left] 'previous-buffer)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)
;;Available Servers
;;-----------------



;;+--------------------+
;;|  File Management   |
;;+--------------------+
;; Backup File
;https://www.emacswiki.org/emacs/backup-each-save.el
;;(load "~/.emacs.d/scripts")
(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;;History
(savehist-mode)

;;Recent Files
(recentf-mode t)

;;+--------------------+
;;|   exp Selection    |
;;+--------------------+
(delete-selection-mode 1)           ;; Replace region when inserting text
(global-subword-mode 1)             ;; Iterate through CamelCase words



;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)


;;Default Settings
;;----------------
(setq visible-bell 1                    ; Annoying Ding off
      ring-bell-function 'ignore
      inhibit-startup-screen t
      initial-major-mode 'org-mode
      debug-on-error t                  ; Debug
      edebug-all-forms t                ; Debug
      undo-limit 200000                 ; Undo Limits
      undo-strong-limit 40000000        ; Undo Limits
      load-prefer-newer t               ; Load Newer el files
      make-pointer-invisible t          ; Hide Mouse While editing
      package-enable-at-startup nil     ; Disable package.el in favor of straight.el
      enable-recursive-minibuffers t    ; Enable recursive minibuffers
      )

;;Line Numbering ;; Disable line numbers for some modes
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
		compilation
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;Auto-Save Management
;;--------------------

;;+-----------------------+
;;|  Package Management   |
;;+-----------------------+

;;Straight
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;Template
;;(straight-use-package '(<package-name> :local-repo "~/.emacs.d/lisp/<package-name>" :type nil))
;;+-------------------+
;;|   Time and date   |
;;+-------------------+
;;https://github.com/alphapapa/ts.el
(use-package ts
  :straight (ts :type git :host github :repo "alphapapa/ts.el"))


;;;
;;+--------------+
;;|  Interface   |
;;+--------------+
(tool-bar-mode 0)                   ;; Disable the tool bar
(scroll-bar-mode 0)                 ;; Disable the scrollbar
;;Mode line controlled by power line
;; (use-package doom-modeline
;;   :straight (doom-modeline :type git :host github :repo "seagle0128/doom-modeline")
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   (set-face-attribute 'doom-modeline nil :family "Fira Code" :height 100
;;   ;;                    'doom-modeline-inactive nil :family "Fira Code" :height 100)
;; 		      )
;;    (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange"))

;;  https://github.com/jwiegley/use-package#diminishing-and-delighting-minor-modes

;;+-----------+
;;|   Theme   |
;;+-----------+
;; (use-package nano-emacs
;;   :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs")
;;   :custom (setq nano-font-family-monospaced "Fira Code Retina"
;; 		nano-font-size 14))
;; (straight-use-package '(nano-emacs :type git
;; 				   :host github
;; 				   :repo "rougier/nano-emacs"))
;; (require 'nano-faces)
;; (require 'nano)
;; (require 'nano-colors)
;; (require 'nano-base-colors)
;; (require 'nano-theme)
;; (nano-dark)
;; (setq nano-font-family-monospaced "Fira Code Retina"
;;       nano-font-size 14)
				   ;; :files (:defaults "/*")
				   ;; :includes (nano
				   ;; 	      nano-colors
				   ;; 	      nano-base-colors
				   ;; 	      nano-bindings
				   ;; 	      nano-command
				   ;; 	      nano-faces
				   ;; 	      nano-agenda
				   ;; 	      nano-faces
				   ;; 	      nano-minibuffer
				   ;; 	      nano-theme
				   ;; 	      nano-writer)))

;; (straight-use-package
;;  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
;; ;;1st Nano Module
;; (require 'nano)


;; ;;Mandatory Nano Modules
;; (require 'nano-base-colors)
;; (require 'nano-faces)

;; ;;Optional Nano Packages
;; (require 'nano-theme-dark)
;; ;; (require 'nano-theme)
;; ;; (nano-theme)
(straight-use-package '(nano-theme :type git :host github
                                   :repo "rougier/nano-theme"))
(load-theme 'nano-dark t)
(set-face-attribute 'default nil :font "Fira Code Retina" :height 130)

(use-package diminish
  :straight (diminish :type git 
		      :host github 
		      :repo "emacsmirror/diminish")
  :config
    (diminish 'abbrev-mode)
    (diminish 'subword-mode)
    (diminish 'eldoc-mode " Ⓔ "))

;;+---------------------+
;;|   Version Control   |
;;+---------------------+
(use-package magit
   :straight (magit :type git :host github :repo "magit/magit")
;;   :diminish auto-revert-mode
   :bind
   (("C-c C-g" . magit-status)
    :map magit-status-mode-map
    ("q"       . magit-quit-session))
   :config
   (setq magit-log-arguments '("--graph"
                               "--decorate"
                               "--color"))

   (setq magit-status-margin
    '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
   (defadvice magit-status (around magit-fullscreen activate)
    "Make magit-status run alone in a frame."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

   (defun magit-quit-session ()
       "Restore the previus window configuration and kill the magit buffer."
       (interactive)
       (kill-buffer)
       (jump-to-register :magit-fullscreen)))

(use-package diff-hl
  :straight (diff-hl :type git :host github :repo "dgutov/diff-hl")
  :init (global-diff-hl-mode)
  :hook (diff-hl-mode . diff-hl-margin-mode))



;;+-----------------+
;;|   Undo & Redo   |
;;+-----------------+
(straight-use-package 'undo-fu)

;;+------------------+
;;|   Key Controls   |
;;+------------------+
(use-package general
  :straight (general :type git :host github :repo "noctuid/general.el")
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-evil-setup t)
  (general-create-definer aviik/leadr-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "C-i")

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   :keymaps 'override

   ;; quitting
   "q" '(:ignore t :which-key "Quit")
   "qq" 'save-buffers-kill-emacs))

(aviik/leadr-key-def
  "fe" '((lambda () (interactive) (find-file "~/.dotfiles/emacs/.emacs.d/init.el"))))



;;+-----------------------+
;;|   Vi Style Workflow   |
;;+-----------------------+

  
(use-package evil
  :straight (evil :type git :host github :repo "emacs-evil/evil")
  :init (setq evil-disable-insert-state-bindings t
	      evil-want-keybinding nil)
  :config (evil-mode))

(use-package evil-collection
  :straight (evil-collection :type git :host github :repo "emacs-evil/evil-collection")
  :after evil
  :init (evil-collection-init)
  :custom (evil-collection-setup-minibuffer t))

(use-package evil-nerd-commenter
  :straight (evil-nerd-commenter :type git
                                 :host github
                                 :repo "redguardtoo/evil-nerd-commenter")
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))


;;+----------------------------+
;;|   Emacs Interface to w3m   |
;;+----------------------------+
(use-package w3m
  :straight (emacs-w3m :type git :host github :repo "emacs-w3m/emacs-w3m")
  :bind (("C-x M-e" . tsa/transient-w3m)
	 :map w3m-mode-map
	 ;;("e" . tsa/transient-w3m)
	 ;;("M-o" . ace-link-w3m)
	 )
  :demand w3m-load
  :custom
  (w3m-search-default-engine "google")
  (w3m-quick-start nil)
  (w3m-display-mode 'plain)
  (w3m-use-title-buffer-name t))


;;+----------------------------------------+
;;|  Icons, Emojis and Special Characters  |
;;+----------------------------------------+
(use-package all-the-icons
             :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el"))


;;+----------------------+
;;|  Snippet Management  |
;;+----------------------+
;;Yasnippet
(straight-use-package 'yasnippet)
(setq yas-snippet-dirs '(
                         "~/.emacs.d/snippets"))
(yas-global-mode t)
(define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
;; use Meta-j and Meta-k to jump between fields
;;(define-key yas-keymap (kbd "M-j") 'yas-next-field-or-maybe-expand)
;;(define-key yas-keymap (kbd "M-k") 'yas-prev-field)

;;+----------------------+
;;|  Completion System   |
;;+----------------------+
;;Vertico
(use-package vertico
  :straight (vertico :type git
		     :host github
		     :repo "minad/vertico"
		     :files (:defaults "extensions/*")
		     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :bind (:map vertico-map
	      ("TAB" . vertico-next)
	      ("<backtab>" . vertico-previous))
  :hook (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle t) 
  :config
  (vertico-mode)
  ;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))


(use-package orderless
  :straight (orderless :type git :host github :repo "oantolin/orderless")
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
   :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
 :init
  (marginalia-mode)
 :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))



;;+-------------------------+
;;|   Completion System     |
;;+-------------------------+
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  :init (global-corfu-mode))

(use-package cape
  :straight (cape :type git :host github :repo "minad/cape")
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345)))

;;+------------------------+
;;|   Parenthesis System   |
;;+------------------------+
;; auto-close parentheses
(electric-pair-mode +1)


;;+---------------------+
;;|   Language Server   |
;;+---------------------+
(use-package lsp-mode
  :straight (lsp-mode :type git
                      :host github
                      :repo "emacs-lsp/lsp-mode")
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (((c-mode
	   c++-mode
	   python-mode
	   rust-mode
	   haskell-mode) . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-ui-mode))
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy"))
  ;;:config
  ;;(setq rustic-lsp-server 'rust-analyzer))

;; optionally
(use-package lsp-ui
  :straight (lsp-ui :type git
                    :host github
                    :repo "emacs-lsp/lsp-ui")
  :commands lsp-ui-mode
  :bind
  (:map lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-j" . lsp-ui-peek-find-definitions)
        ("C-c i"   . lsp-ui-peek-find-implementation)
        ;;("C-c m"   . lsp-ui-imenu)
        ("C-c s"   . lsp-ui-sideline-mode)
        ("C-c d"   . aviik/toggle-lsp-ui-doc))
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-code-actions-prefix ""))

(use-package company
  :straight (company :type git
                     :host github
                     :repo "company-mode/company-mode")
  :diminish (company-mode  .  " Ⓒ ")
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :init
  (global-company-mode)
  :config (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))
(add-to-list 'company-backends '(company-ghci
				 company-shell
				 company-shell-env
				 company-fish-shell))

(use-package company-box
  :straight (company-box :type git :host github :repo "sebastiencs/company-box")
  :hook (company-mode . company-box-mode))


(use-package company-ghci
  :straight (company-ghci :type git :host github :repo "horellana/company-ghci"))

(use-package company-shell
  :straight (company-shell :type git :host github :repo "Alexander-Miller/company-shell"))



;;+-----------------------------------+
;;|   Terminals & External Payloads   |
;;+-----------------------------------+
(use-package term-toggle
  :straight (term-toggle :type git :host github :repo "amno1/emacs-term-toggle")
  :bind ("<f2>" . term-toggle-eshell)
  :config (setq term-toggle-kill-buffer-on-term-exit t))

(use-package vterm
  :straight (vterm :type git :host github :repo "akermu/emacs-libvterm")
  :commands (vterm vterm-other-window)
  )



;;Runstuff
(use-package run-stuff
  :straight
  (run-stuff
    :files (:defaults "run-stuff")
    :host nil
    :type git
    :repo "https://codeberg.org/ideasman42/emacs-run-stuff.git"))

;;Prodigy

(use-package prodigy
  :straight (prodigy :type git
                     :host github
                     :repo "rejeep/prodigy.el")
  :defer 1
  :commands prodigy
  :bind (("<f12>" . prodigy)))

;;Path to Shell
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :type git
				  :host github
				  :repo "purcell/exec-path-from-shell")

  :ensure
  :init (exec-path-from-shell-initialize))

;;+------------+
;;|   Parser   |
;;+------------+

;((require 'tree-sitter)
;(require 'tree-sitter-langs)

;;(global-tree-sitter-mode)


;;+----------------------------------+
;;|   Programming Language Support   |
;;+----------------------------------+
;;E-Lisp Develoment - The missing hash table library for Emacs.
(use-package ht
  :straight (ht :type git :host github :repo "Wilfred/ht.el"))

;;Elisp -Formatter
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode)
  :hook (emacs-lisp-mode . elisp-autofmt-mode)
  :straight
  (elisp-autofmt
    :files (:defaults "elisp-autofmt")
    :host nil
    :type git
    :repo "https://codeberg.org/ideasman42/emacs-elisp-autofmt.git"))

;;Rust
;;------
(use-package rustic
  :straight (rustic :type git :host github :repo "brotzeit/rustic")
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package lsp-haskell
    :straight (lsp-haskell :type git :host github :repo "emacs-lsp/lsp-haskell"))



;;Haskell
(use-package haskell-mode
  :straight (haskell-mode :type git :host github :repo "haskell/haskell-mode")
  ;; :init((custom-set-variables
  ;;         '(haskell-process-suggest-remove-import-lines t)
  ;;         '(haskell-process-auto-import-loaded-modules t)
  ;;         '(haskell-process-log t)))
  :bind (:map haskell-mode-map
	      ("C-c h" . hoogle)
	      )
  :hook ((haskell-mode . interactive-haskell-mode)
	 (haskell-mode . (lambda ()
                           (set (make-local-variable 'company-backends)
                                (append '((company-capf company-dabbrev-code))
                                        company-backends))))
	 ;;(haskell-mode . haskell-indentation-mode)
	 (haskell-mode . turn-on-haskell-unicode-input-method)
	 ;;(haskell-mode . flyspell-prog-mode)
	 )
  :config
  (setq ;;lsp-enable-symbol-highlighting nil
       ;;lsp-signature-auto-activate nil
       haskell-stylish-on-save t))

;; (add-to-list 'align-rules-list
;;              '(haskell-types
;;                (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
;;                (modes quote (haskell-mode literate-haskell-mode))))
;; (add-to-list 'align-rules-list
;;              '(haskell-assignment
;;                (regexp . "\\(\\s-+\\)=\\s-+")
;;                (modes quote (haskell-mode literate-haskell-mode))))
;; (add-to-list 'align-rules-list
;;              '(haskell-arrows
;;                (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
;;                (modes quote (haskell-mode literate-haskell-mode))))
;; (add-to-list 'align-rules-list
;;              '(haskell-left-arrows
;;                (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
;;                (modes quote (haskell-mode literate-haskell-mode))))


;;GD Script
;;----------
(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

;;Major mode for editing JSON files of any size
(use-package jsonian
  :straight (jsonian :type git :host github :repo "iwahbe/jsonian")
  :after so-long  
  :custom
  (jsonian-no-so-long-mode))


;;+---------------+
;;|   Debugging   |
;;+---------------+
(with-eval-after-load 'rustic-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;+-------------------+
;;|   Bug Reproduce   |
;;+-------------------+
(straight-use-package '(yodel :host github :repo "progfolio/yodel"))

;; test model format
;; (yodel
;;   :post*
;;   (yodel-file
;;     :point "|"
;;     :with* "test: |fail"
;;     :then*
;;     (kill-word 1)
;;     (insert "pass")
;;     (message "%s" (buffer-string))))




(use-package treemacs
  :straight (treemacs :type git
		      :host github
		      :repo "Alexander-Miller/treemacs"
		      :files (:defaults "src/*/*") ;;"src/elisp/*" "src/scripts/*")
		      :includes (treemacs-evil
				 treemacs-all-the-icons
				 treemacs-magit))
  :defer t
  :config
   (general-define-key
    :keymaps 'treemacs-mode-map
    [mouse-1] #'treemacs-single-click-expand-action
    "M-l" #'treemacs-root-down
    "M-h" #'treemacs-root-up
    "q" #'treemacs-quit)
   (general-define-key
    :keymaps 'treemacs-mode-map
    :states '(normal emacs)
    "q" 'treemacs-quit))

(use-package lsp-treemacs
  :straight (lsp-treemacs :type git :host github :repo "emacs-lsp/lsp-treemacs")
  :after (treemacs)
  :config
  (lsp-treemacs-sync-mode 1))



(use-package which-key
    :config
    (which-key-mode))


;;+----------------------+
;;|   Org-Mode Support   |
;;+----------------------+
(straight-use-package '(org :type built-in))


(use-package org
  :ensure org-plus-contrib
  :hook (org-mode . aviikc/org-mode-setup)
;;  :custom
  :config (message "Loading Org.........")
)

(defun aviik/org-mode-font-faces ()
;;  (setq-local org-hidden-keywords '(title author date startup))
  (setq-default line-spacing 1)
  (setq org-startup-indented t
	org-directory "~/OneDrive/org"
	;;Todo keywords I need
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(n)" "|" "DONE(d)" "CANCELED(c@)"))
	org-export-allow-bind-keywords t
	org-fontify-done-headline t
	;; org-hide-leading-stars t
	;; org-num-skip-unnumbered t
	;; org-num-skip-footnotes t
	;; org-num-max-level 2
	;; org-num-face nil
	header-line-format nil
	fill-column 72))

(defun aviikc/org-mode-setup()
  (aviik/org-mode-font-faces)
  (org-indent-mode)
;;  (org-num-mode)
  (variable-pitch-mode)
  (visual-line-mode)
  )



(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator #x2501
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)


(defun my/set-specific-faces-org ()
  (set-face-attribute 'org-code nil
                      :inherit '(shadow fixed-pitch))
  ;; Without indentation the headlines need to be different to be visible
  (set-face-attribute 'org-level-1 nil
                      :height 1.25
                      :foreground "#BEA4DB")
  (set-face-attribute 'org-level-2 nil
                      :height 1.15
                      :foreground "#A382FF"
                      :slant 'italic)
  (set-face-attribute 'org-level-3 nil
                      :height 1.1
                      :foreground "#5E65CC"
                      :slant 'italic)
  (set-face-attribute 'org-level-4 nil
                      :height 1.05
                      :foreground "#ABABFF")
  (set-face-attribute 'org-level-5 nil
                      :foreground "#2843FB")
  (set-face-attribute 'org-date nil
                      :foreground "#ECBE7B"
                      :height 0.8)
  (set-face-attribute 'org-document-title nil
                      :foreground "DarkOrange3"
                      :height 1.3)
  (set-face-attribute 'org-ellipsis nil
                      :foreground "#4f747a" :underline nil)
  (set-face-attribute 'variable-pitch nil
                      :family "Roboto Slab" :height 1.2))
(defun my/set-keyword-faces-org ()
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        '(;; Syntax
          ("TODO" .     "")
          ("DONE" .     "")
          ("WAITING" .  "")
          ("HOLD" .     "")
          ("NEXT" .     "")
          ("CANCELLED" . "")
          ("#+begin_quote" . "“")
          ("#+end_quote" . "”")))
  (prettify-symbols-mode +1)
  )


(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (lisp . t))))



;;(remove-hook 'window-configuration-change-hook #'nano-modeline-update-windows)

(use-package org-sidebar
  :straight (org-sidebar :type git :host github :repo "alphapapa/org-sidebar")
  :after org-mode)

;;https://github.com/alphapapa/org-super-agenda#installation
(use-package org-super-agenda
  :straight (org-super-agenda :type git :host github :repo "alphapapa/org-super-agenda")
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-groups
        '((:name "Work" :tag "work" :order 1)
          (:name "In Progress" :todo "IN-PROGRESS" :order 1)
          (:name "Projects" :tag "project" :order 3)
          (:name "Home" :tag "home" :order 2)
          (:name "Study" :tag "study" :order 4)
          (:name "Habits" :tag "habit" :order 5))))







(use-package evil-org
  :straight (evil-org :type git :host github :repo "Somelauw/evil-org-mode")
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config  (require 'evil-org-agenda)
           (evil-org-agenda-set-keys))

;;https://github.com/tecosaur/org-pandoc-import
(use-package org-pandoc-import
  :straight (:host github
             :repo "tecosaur/org-pandoc-import"
             :files ("*.el" "filters" "preprocessors")))


;;;Hugo
;;;Ox-hugo
;;;https://ox-hugo.scripter.co/
(use-package ox-hugo
  :straight(ox-hugo :type git
                    :host github
                    :repo "kaushalmodi/ox-hugo")
  :after ox)

(use-package ox-json
  :straight (ox-json :type git :host github :repo "jlumpe/ox-json")
  :after ox)




;;;; ===========================
;;;; Publishing with Org Mode
;;;; ===========================

;;;Presentations with Org-Reveal
;;;https://github.com/yjwen/org-reveal
(use-package ox-reveal
  :straight (ox-reveal :type git :host github :repo "yjwen/org-reveal"))



;;(require 'nano-help)

;;Mode line controlled by power line
(use-package doom-modeline
  :straight (doom-modeline :type git :host github :repo "seagle0128/doom-modeline")
  :hook (after-init . doom-modeline-mode)
  :config
  ;; (set-face-attribute 'doom-modeline nil :family "Fira Code" :height 100
  ;; ;;                    'doom-modeline-inactive nil :family "Fira Code" :height 100)
  ;; 		      )
   (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange"))
(menu-bar-mode -1)


(setq org-publish-project-alist
      '(("Org-Json"
         :base-directory "~/OneDrive/org/"
         :base-extension "org"
         :publishing-directory "~/OneDrive/dev/a_data"
         :recursive t
         :publishing-function ox-json-export-to-file)))

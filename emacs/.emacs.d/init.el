;; -*- lexical-binding: t; -*-

;; Some litrature
;; cutting is called “killing”.
;; Pasting is called “yanking”.
;; Buffers and windows basically mean the same thing in emacs.
;; Frame is a system-level window.



;; (setq make-backup-files nil)
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@args))




;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)



;; (setq-default display-line-numbers 'visual
;;               display-line-numbers-widen t
;;               ;; this is the default
;;               display-line-numbers-current-absolute t)





;;;;;;;;;;;;;
;; GENERAL ;;
;;;;;;;;;;;;;
(use-package general
  :demand
  :config
  (general-evil-setup)
  (general-override-mode)
  (general-auto-unbind-keys)

(general-define-key
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator emacs)
 :prefix-map '+prefix-map
 :prefix "SPC"
 :global-prefix "S-SPC")

(general-define-key
 :keymaps 'org-mode-map
 :prefix "C-c"
 ;; bind "C-c a" to 'org-agenda
 "a" 'org-agenda
 "b" 'counsel-bookmark
 "c" 'org-capture)
;; general.el can automate the process of prefix map/command creation
(general-nmap
  :prefix "SPC"
  :prefix-map 'my-leader-map
  "eb" 'eval-buffer
  "," 'list-buffers))



(use-package diminish
  :demand
  :config
  (diminish 'linum-relative-mode)
  (diminish 'eldoc-mode))

(elpaca-process-queues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS-DEFAULT-SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :elpaca nil
  :general
  ('normal "gcc" 'comment-line)
  ('visual "gc" 'comment-or-uncomment-region)
  ('insert "C-V" 'yank) 
  ;; :hook
  ;; ( prog-mode . display-line-numbers-mode)
  :config
  ;; (setq-default display-line-numbers 'visual
  ;;             display-line-numbers-widen t
  ;;             ;; this is the default
  ;;             display-line-numbers-current-absolute t)
  ;; corfu
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)
  (delete-selection-mode 1)
  (setq ring-bell-function #'ignore
	confirm-kill-processes nil           ;; disable confirmation to kill processes on Emacs exit
	;; display-line-numbers-type 'relative
	inhibit-startup-screen t
	completion-cycle-threshold 3
	warning-minimum-level :error)
  (setq-default
   enable-recursive-minibuffers t
   kill-buffer-query-functions nil))

(custom-set-faces '(line-number-current-line ((t :weight bold
                                                 :foreground "goldenrod"
                                                 :background "slate gray"))))

  ;; do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; don't need to confirm to revert buffer
  (setq revert-without-query t)
;;;;;;;;;;;;;;;;;;;;;;
;; GLYPHS AND ICONS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; (use-package all-the-icons
;;   :if (display-graphic-p))
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package nerd-icons
    :elpaca (nerd-icons
             :host github
             :repo "rainstormstudio/nerd-icons.el"
             :files (:defaults "data"))
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "JetBrains Mono NF")
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(set-face-attribute 'default nil
  :font "JetBrainsMono Nerd Font"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Vollkorn"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono Nerd Font"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-adjust)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)


(use-package pixel-scroll
  :elpaca nil
  :if (string-greaterp emacs-version "29") ; need emacs > 29
  :defer t 
  :config
  ;; text scroll pixel by pixel
  (pixel-scroll-precision-mode t))

;;;;;;;;;;;
;; THEME ;;
;;;;;;;;;;;
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)
  (load-theme 'doom-tokyo-night t)) 


;;;;;;;;;;;;;;
;; KEYBOARD ;;
;;;;;;;;;;;;;;
(use-package which-key
  :config
  (which-key-mode))


;;;;;;;;;;;;;;;;;
;; PARENTHESIS ;;
;;;;;;;;;;;;;;;;;


(use-feature paren
  :defer 1
  :config (show-paren-mode))



(use-package undo-tree
  :init
  (global-undo-tree-mode 1))



(use-package zoxide
  :elpaca (zoxide :fetcher gitlab :repo "Vonfry/zoxide.el")
  :general
  ;; ("C-c z" 'dired-jump-with-zoxide)
  (:states 'normal :no-autoload t
	   "SPC gz" 'zoxide-find-file-with-query
	   "SPC gZ" 'zoxide-find-file-with-query)
  :preface 
  (defun dired-jump-with-zoxide (&optional other-window)
    (interactive "P")
    (zoxide-find-file-with-query nil (lambda (file) (dired-jump other-window file)) t)))



;;;;;;;;;;
;; EVIL ;;
;;;;;;;;;;
(defun noct-relative ()
  "Show relative line numbers."
  (setq-local display-line-numbers 'visual))
	  
(defun noct-absolute ()
  "Show absolute line numbers."
  (setq-local display-line-numbers t))
;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :demand t
  :init (setq evil-want-keybinding nil
	      evil-want-minibuffer t)
    (setq-default display-line-numbers 'visual
              display-line-numbers-widen t
              ;; this is the default
              display-line-numbers-current-absolute t)

  :custom
  (evil-want-integration t)
  (evil-undo-system 'undo-tree)
  :general
  ('(normal visual) "[ ]" 'evil-next-close-paren)
  ('(normal visual) "] [" 'evil-previous-open-paren)
  ('normal "j" 'evil-next-visual-line)
  ('normal "k" 'evil-previous-visual-line)
  :hook ( (evil-insert-state-entry . noct-absolute)
	  (evil-insert-state-exit . noct-relative))
  :config
  (define-key evil-motion-state-map [down-mouse-1] nil)
  (evil-mode))

(use-package evil-anzu
  :after (evil anzu))

(use-package evil-collection
  :ensure t
  :after (evil)
  :config (evil-collection-init)
  :custom
  (evil-collection-elpaca-want-g-filters nil)
  (evil-collection-setup-minibuffer t "Add evil bindings to minibuffer")
  (evil-collection-ement-want-auto-retro t))

;; https://github.com/tpope/vim-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; visualize evil commands
(use-package evil-goggles
  :diminish evil-goggles-mode
  :after evil
  :defer 1
  :config
  (evil-goggles-mode)
  (setq
   evil-goggles-duration 0.8        ; show what I copied
        evil-goggles-async-duration 0.8  ; affects indenting
        evil-goggles-blocking-duration 0) ; don't want to wait when deleting
  (evil-goggles-use-diff-faces))


(use-package evil-terminal-cursor-changer
  :elpaca (evil-terminal-cursor-changer :host github :repo "7696122/evil-terminal-cursor-changer")
  :config 
  (unless (display-graphic-p)
          (require 'evil-terminal-cursor-changer)
          (evil-terminal-cursor-changer-activate) ; or (etcc-on)
          ))
(use-package evil-cleverparens
  :elpaca (evil-cleverparens :host github :repo "emacs-evil/evil-cleverparens")
  :hook (emacs-lisp-mode . evil-cleverparens-mode))

(use-package anzu
  :defer 10
  :config (global-anzu-mode))


;; (defun spook--aw-kill-buffer-in-window (win)
;;   "Kill the buffer shown in window WIN."
;;   (kill-buffer (window-buffer win)))

;; (defun spook--aw-kill-buffer-and-window (win)
;;   "Kill the buffer shown in window WIN and window itself."
;;   (kill-buffer (window-buffer win))
;;   (delete-window win))

;; (spook--defkeymap "spook-windows" "C-c s-w"
;;   '("-" . split-window-below)
;;   '("_" . spook--baby-window)
;;   '("/" . split-window-right)
;;   '("d" . delete-window)
;;   '("m" . delete-other-windows)
;;   '("o" . other-window)
;;   '("h" . windmove-left)
;;   '("j" . windmove-down)
;;   '("k" . windmove-up)
;;   '("l" . windmove-right)
;;   '("w" . ace-window))
        

(use-package ace-window
  :bind ("M-p" . ace-window)
    :config
  (setq aw-dispatch-always t)
  (global-set-key (kbd "C-c w") 'ace-window)
  (setq aw-dispatch-alist
        '(
	  ;; (?d spook--aw-kill-buffer-in-window "Kill buffer in window")
          (?s aw-swap-window "Swap Windows")
          (?S aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?w aw-flip-window)
          (?b aw-switch-buffer-in-window "Select Buffer")
          (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?k aw-delete-window "Delete Window")
          ;; (?K spook--aw-kill-buffer-and-window "Kill buffer in window")
          (?= aw-split-window-fair "Split Fair Window")
          (?- aw-split-window-vert "Split Vert Window")
          (?/ aw-split-window-horz "Split Horz Window")
          (?m delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help))
        aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))






;;;;;;;;;;;;;;;;;;;
;; DOOM-MODELINE ;;
;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon t
	;; doom-modeline-major-mode-icon t   ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
	doom-modeline-modal-icon t
	doom-modeline-buffer-modification-icon t))

;; (elpaca-queue (elpaca '(melpulls :host github :repo "progfolio/melpulls")
;;                       (add-to-list 'elpaca-menu-functions #'melpulls)))



;;;;;;;;;;;;;;
;; SNIPPETS ;;
;;;;;;;;;;;;;;

;; Configure Tempel
(use-package tempel
  :elpaca (tempel :host github :repo "minad/tempel")
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  (prog-mode . tempel-setup-capf)
  (text-mode . tempel-setup-capf)


  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :elpaca (tempel-collection :host github :repo "Crandel/tempel-collection"))


;;;;;;;;;;;;;;;;
;; COMPLETION ;;
;;;;;;;;;;;;;;;;

(use-package corfu
  :elpaca (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-popupinfo-delay . 0.3)
  :init
  (global-corfu-mode)
    :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
    :config
  ;; (global-corfu-mode)
  (with-eval-after-load 'evil
    (setq evil-complete-next-func (lambda (_) (completion-at-point)))))
;; (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; (use-package corfu-terminal
;;   :elpaca (corfu-terminal :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
;;   :config
;;   (unless (display-graphic-p)
;;   (corfu-terminal-mode +1)))

;; (use-package corfu-doc-terminal
;;  :elpaca (corfu-doc-terminal :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
;;  :config 
;; 	  (unless (display-graphic-p) (corfu-doc-terminal-mode +1)))


(use-package cape
  :init
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
(use-package smartparens
  :diminish smartparens-mode
  :hook
  (prog-mode . smartparens-mode)
  (LaTeX-mode . smartparens-mode)
  (nxml-mode . smartparens-mode))

(use-package vertico
  :elpaca (vertico :type git :host github :repo "minad/vertico"
                     :files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  (setq vertico-resize t))

(use-package vertico-directory
  :elpaca nil
  :after vertico
  :general
  (vertico-map "RET" 'vertico-directory-enter
               "DEL" 'vertico-directory-delete-char
               "M-DEL" 'vertico-directory-delete-word)
  ;; tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;; repeat last vertico session
(use-package vertico-repeat
  :elpaca nil
  :after vertico
  :general
  ("M-r" 'vertico-repeat))


;; (defun my/eglot-capf ()
 ;;   (setq-local completion-at-point-functions
 ;;               (list (cape-super-capf
 ;;                      #'eglot-completion-at-point
 ;;                      #'tempel-expand
 ;;                      #'cape-file))))

;; (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)


;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;; (use-package company
;;   :ensure t
;;   :hook (after-init . global-company-mode)
;;   :config (setq company-idle-delay 0
;; 		company-minimum-prefix-length 1)
;;   (company-tng-configure-default))

(use-package magit
  :ensure t
  :bind ( "C-x g" . magit-status))

;; shows git information on fringe
(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode)
  ;; (dired-mode . diff-hl-dired-mode)
  ;; integration with magit
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))



(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))


(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package flycheck
  :init (global-flycheck-mode))


(use-package perspective-el
  :elpaca (perspective-el :host github :repo "nex3/perspective-el")
    :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))


(use-package tree-sitter
    :config
  ;; (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;; use tree sitter as evil text objects
(use-package evil-textobj-tree-sitter :disabled
  :elpaca (evil-textobj-tree-sitter :type git
                                      :host github
                                      :repo "meain/evil-textobj-tree-sitter"
                                      :files (:defaults "queries"))
  :after tree-sitter
  :config
  (define-key evil-outer-text-objects-map "s" (evil-textobj-tree-sitter-get-textobj "statement.outer"))
  (define-key evil-inner-text-objects-map "n" (evil-textobj-tree-sitter-get-textobj "scopename.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "call.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))



(use-package cmake
  :elpaca nil
  :mode ("\\CMakeLists.txt\\'" . cmake-ts-mode))

(use-package rust-mode
  :hook ( rust-mode . rust-ts-mode))


(use-package python
  :bind (:map python-ts-mode-map
              ("<f5>" . recompile)
              ("<f6>" . eglot-format))
  :hook ((python-ts-mode . eglot-ensure)
         )
  :mode (("\\.py\\'" . python-ts-mode)))

;; edit .toml files
(use-package toml-mode
  :mode "\\.toml")

;; edit .yaml files
(use-package yaml-mode
  :mode "\\.yaml")


(defun +aviik-pdf-viewer ()
  (advice-add #'TeX-command-master :before (lambda (&rest r) (save-buffer)))
  (push (list 'output-pdf "Zathura") TeX-view-program-selection))

(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode . +aviik-pdf-viewer))



(use-package highlight-indent-guides
  :ensure t
  :hook (python-ts-mode . highlight-indent-guides-mode)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "white")
  (setq highlight-indent-guides-method 'character))







(use-package eglot
  :elpaca nil
  :custom-face
  (eglot-highlight-symbol-face ((t (:underline t :weight bold))))
  :bind (:map eglot-mode-map
              ("C-c d" . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c f" . flymake-show-buffer-diagnostics)
              ("C-c r" . eglot-rename))
  :hook ((c-mode        .      eglot-ensure)
	 (c++-mode      .      eglot-ensure)
	 (cmake-ts-mode .      eglot-ensure)
	 (rust-mode     .      eglot-ensure)))







(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package vterm
    :ensure t)


;; (use-package emacs-coterm
;;   ;; :ensure t)
;;   :elpaca (emacs-coterm :type git
;; 			:repo "https://repo.or.cz/emacs-coterm.git"))


(use-package mistty
  :elpaca (mistty :host github :repo "szermatt/mistty")
  :hook (mistty-mode . turn-off-evil-mode)
  :bind (("C-c x m" . mistty)

         ;; bind here the shortcuts you'd like the
         ;; shell to handle instead of Emacs.
         :map mistty-prompt-map

         ;; fish: directory history
         ("M-<up>" . mistty-send-key)
         ("M-<down>" . mistty-send-key)
         ("M-<left>" . mistty-send-key)
         ("M-<right>" . mistty-send-key)))

(defun jdm204-eat-bash (arg)
  "run 'eat' with bash as the shell"
  (interactive "P")
  (let ((bashpath "/bin/bash"))
  (if (equal current-prefix-arg '(4))
      (eat bashpath t)
    (eat bashpath))))

(defun jdm204-eat-nu (arg)
  "run 'eat' with nu as the shell"
  (interactive "P")
  (let ((nupath "~/.cargo/bin/nu"))
  (if (equal current-prefix-arg '(4))
      (eat nupath t)
    (eat nupath))))

(use-package eat
  :elpaca (eat :type git
               :host codeberg
               :repo "akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el")))
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-eshell-mode t)
  :bind
  (:map eat-semi-char-mode-map
   ("M-o" . crux-other-window-or-switch-buffer))
  (:map global-map
  ("<C-i> b" . jdm204-eat-bash)
  ("<C-i> n" . jdm204-eat-nu)
  ("<C-i> e" . eshell)))




(elpaca-wait)

(use-package org
  :elpaca nil
  :ensure nil
  :hook
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  :config
  (setq
   ;; Edit settings
     org-auto-align-tags nil
     org-tags-column 0
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     org-insert-heading-respect-content t

     ;; Org styling, hide markup etc.
     org-hide-emphasis-markers t
     org-pretty-entities t
     org-ellipsis "…"

     ;; Agenda styling
     org-agenda-tags-column 0
     org-agenda-block-separator ?─
     org-agenda-time-grid
     '((daily today require-timed)
       (800 1000 1200 1400 1600 1800 2000)
       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
     org-agenda-current-time-string
     "now ─────────────────────────────────────────────────")
  (setq org-startup-indented t
      org-pretty-entities t
      org-startup-with-inline-images t
      org-startup-folded t 
      org-image-actual-width '(300))
  (electric-indent-mode -1)
  (setq org-edit-src-content-indentation 0)
  (require 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (emacs-lisp . t))))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))
(use-package org-roam
  :after org
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/OneDrive/org/OrgRoam"))
  ;; :general
  ;; ("C-c n l" 'org-roam-buffer-toggle)
  ;; ("C-c n f" 'org-roam-node-find)
  ;; ("C-c n g" 'org-roam-graph)
  ;; ("C-c n i" 'org-roam-node-insert)
  ;; ("C-c n c" 'org-roam-capture)
  ;; ;; Dailies
  ;;   ("C-c n j" 'org-roam-dailies-capture-today)
    :config
    ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-autosync-mode))

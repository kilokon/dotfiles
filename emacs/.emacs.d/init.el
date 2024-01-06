;; -*- lexical-binding: t; -*-

;; Some litrature
;; cutting is called “killing”.
;; Pasting is called “yanking”.
;; Buffers and windows basically mean the same thing in emacs.
;; Frame is a system-level window.

;; make ibuffer default
(defalias 'list-buffers 'ibuffer-other-window)

;; (setq make-backup-files nil)
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory
  (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory
  (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory
  (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca
    :repo "https://github.com/progfolio/elpaca.git"
    :ref nil
    :files (:defaults (:exclude "extensions"))
    :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list
   'load-path
   (if (file-exists-p build)
       build
     repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer
                  (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop
                   (call-process "git"
                                 nil
                                 buffer
                                 t
                                 "clone"
                                 (plist-get order :repo)
                                 repo)))
                 ((zerop
                   (call-process "git"
                                 nil
                                 buffer
                                 t
                                 "checkout"
                                 (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop
                   (call-process
                    emacs
                    nil
                    buffer
                    nil
                    "-Q"
                    "-L"
                    "."
                    "--batch"
                    "--eval"
                    "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
          (progn
            (message "%s" (buffer-string))
            (kill-buffer buffer))
          (error
           "%s"
           (with-current-buffer buffer
             (buffer-string))))
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
  `(use-package ,name :elpaca nil ,@args))


;; Install use-package support
(elpaca
 elpaca-use-package
 ;; Enable :elpaca use-package keyword.
 (elpaca-use-package-mode)
 ;; Assume :elpaca t unless otherwise specified.
 (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)
(message "loaded elpaca basics")

;; (setq-default display-line-numbers 'visual
;;               display-line-numbers-widen t
;;               ;; this is the default
;;               display-line-numbers-current-absolute t)


;;;;;;;;;;;;;;
;; KEYBOARD ;;
;;;;;;;;;;;;;;
(use-package which-key :config (which-key-mode))


;;;;;;;;;;;;;
;; GENERAL ;;
;;;;;;;;;;;;;
(use-package
 general
 :demand
 :config
 (general-evil-setup)
 (general-override-mode)
 (general-auto-unbind-keys)

 ;; ;; Leader Key
 ;;  (general-create-definer
 ;;   my-leader-def
 ;;   ;; :prefix my-leader
 ;;   :prefix "SPC")
 ;;  ;; general.el can automate the process of prefix map/command creation

 (general-nmap
  :prefix "SPC"
  :prefix-map
  'my-leader-map
  "eb"
  'eval-buffer
  "ff"
  'find-file
  "qq"
  'kill-buffer
  "bt"
  'switch-to-buffer-other-tab
  ;; 'switch-to-buffer-other-window
  "bb"
  'switch-to-buffer)

 (general-define-key
  :keymaps 'org-mode-map
  :prefix
  "C-c"
  "a"
  'org-agenda
  "b"
  'counsel-bookmark
  "c"
  'org-capture))
;; general.el can automate the process of prefix map/command creation

(use-package
 major-mode-hydra
 :elpaca
 (major-mode-hydra :host github :repo "jerrypnz/major-mode-hydra.el")
 :bind ("M-SPC" . major-mode-hydra))

(use-package
 diminish
 :demand t
 :config
 (diminish 'linum-relative-mode)
 (diminish 'eldoc-mode))

(use-package
 shackle
 :ensure t
 :config (shackle-mode) (setq shackle-default-rule '(:same t))
 (setq shackle-rules
       '((compilation-mode :select t :size 0.4)
         ("\\`\\*Just.*?\\*\\'" :regexp t :align t :size 0.4)
         ("\\`\\*Anzu.*?\\*\\'" :regexp t :align t :size 0.4))))


(elpaca-process-queues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS-DEFAULT-SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
 emacs
 :elpaca nil
 :preface
 (defun my-display-numbers-hook ()
   (display-line-numbers-mode t))
 :hook
 (prog-mode . electric-pair-mode)
 (prog-mode . my-display-numbers-hook)
 ;; :general
 ;; ('normal "gcc" 'comment-line)
 ;; ('visual "gc" 'comment-or-uncomment-region)
 ;; ('insert "C-V" 'yank) 
 ;; :hook
 ;; ( prog-mode . display-line-numbers-mode)
 :config
 ;; (tab-bar-mode 1)
 ;; (setq-default display-line-numbers 'visual
 ;;             display-line-numbers-widen t
 ;;             ;; this is the default
 ;;             display-line-numbers-current-absolute t)
 ;; corfu
 (setq completion-cycle-threshold 3)
 (setq read-extended-command-predicate
       #'command-completion-default-include-p)
 (delete-selection-mode 1)
 (setq
  ring-bell-function #'ignore
  confirm-kill-processes nil ;; disable confirmation to kill processes on Emacs exit
  ;; display-line-numbers-type 'relative
  inhibit-startup-screen t
  completion-cycle-threshold 3
  warning-minimum-level
  :error)
 (setq-default
  ;; white-space
  ;; Show stray whitespace.
  show-trailing-whitespace t
  indicate-empty-lines t
  indicate-buffer-boundaries 'left
  ;; minibuffer
  enable-recursive-minibuffers t
  kill-buffer-query-functions nil)
 ;; Search back/forth for the symbol at point
 ;; See http://www.emacswiki.org/emacs/SearchAtPoint
 (defun isearch-yank-symbol ()
   "*Put symbol at current point into search string."
   (interactive)
   (let ((sym (thing-at-point 'symbol)))
     (if sym
         (progn
           (setq
            isearch-regexp t
            isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
            isearch-message
            (mapconcat 'isearch-text-char-description isearch-string
                       "")
            isearch-yank-flag t))
       (ding)))
   (isearch-search-and-update)))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(evil-goggles-change-face ((t (:inherit diff-removed))))
;;  '(evil-goggles-delete-face ((t (:inherit diff-removed))))
;;  '(evil-goggles-paste-face ((t (:inherit diff-added))))
;;  '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
;;  '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
;;  '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
;;  '(evil-goggles-yank-face ((t (:inherit diff-changed))))
;;  '(line-number-current-line ((t :weight bold :foreground "goldenrod"))))

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
(use-package
 kind-icon
 :ensure t
 :after corfu
 :custom
 (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
 :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package
 nerd-icons
 :elpaca
 (nerd-icons
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
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list
 'default-frame-alist '(font . "JetBrainsMono Nerd Font-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-adjust)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)


(message "loaded emacs icons")


(use-package
 pixel-scroll
 :elpaca nil
 :if (string-greaterp emacs-version "29") ; need emacs > 29
 :defer t
 :config
 ;; text scroll pixel by pixel
 (pixel-scroll-precision-mode t))

;;;;;;;;;;;
;; THEME ;;
;;;;;;;;;;;
(use-package
 doom-themes
 :ensure t
 :custom-face
 (font-lock-comment-face
  ((t (:slant italic) (:background unspecified))))
 :config
 ;; Global settings (defaults)
 (setq
  doom-themes-enable-bold t ; if nil, bold is universally disabled
  doom-themes-enable-italic t)
 (load-theme 'doom-tokyo-night t))


;; Must be used *after* the theme is loaded
;; (custom-set-faces
;;  `(font-lock-comment-face ((t (:background unspecified)))))


;; (message "loaded doom-themes")


;;;;;;;;;;;;;;;;;
;; PARENTHESIS ;;
;;;;;;;;;;;;;;;;;


(use-feature paren
  :defer 1
  :config (show-paren-mode))


(use-package
 undo-tree
 :init (global-undo-tree-mode 1)
 :config (setq undo-tree-show-minibuffer-help t))


(use-package
 zoxide
 :elpaca (zoxide :fetcher gitlab :repo "Vonfry/zoxide.el")
 :general
 ("C-c z" 'dired-jump-with-zoxide)
 (:states 'normal :no-autoload t "zz" 'zoxide-find-file-with-query)
 :preface
 (defun dired-jump-with-zoxide (&optional other-window)
   (interactive "P")
   (zoxide-find-file-with-query
    nil
    (lambda (file) (dired-jump other-window file)) t)))


(use-package
 centaur-tabs
 :elpaca t
 :demand t
 :config
 (centaur-tabs-mode t)
 (setq
  centaur-tabs-style "bar"
  centaur-tabs-height 32
  centaur-tabs-set-icons t)
 :bind
 ("C-<prior>" . centaur-tabs-backward)
 ("C-<next>" . centaur-tabs-forward)
 ;; :general
 ;; ('normal "g t" 'centaur-tabs-forward)
 ;; ('normal "g T" 'centaur-tabs-backward)
 )

(message "loaded tabs")
;;;;;;;;;;
;; CRUX ;;
;;;;;;;;;;
;; (use-package crux :elpaca (crux :host github :repo "bbatsov/crux"))


;;;;;;;;;;;;;;;;;;
;; CODE-FOLDING ;;
;;;;;;;;;;;;;;;;;;

(use-package origami :elpaca t)

;; add a visual intent guide
(use-package
 highlight-indent-guides
 :ensure t
 :hook (emacs-lisp-mode . highlight-indent-guides-mode)
 :init
 ;; (setq highlight-indent-guides-method 'column)
 ;; (setq highlight-indent-guides-method 'bitmap)
 (setq highlight-indent-guides-method 'character)
 (setq highlight-indent-guides-character ?‖)
 ;; (setq highlight-indent-guides-responsive 'top)
 (setq highlight-indent-guides-responsive 'stack)
 (setq highlight-indent-guides-auto-enabled nil)
 :config
 (set-face-background 'highlight-indent-guides-odd-face "darkgray")
 (set-face-background 'highlight-indent-guides-even-face "dimgray")
 (set-face-foreground
  'highlight-indent-guides-character-face "dimgray"))


;;;;;;;;;;;;;;;
;; RYO-MODAL ;;
;;;;;;;;;;;;;;;
;; (use-package misc
;; :ensure t
;; :commands forward-to-word)


;; Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.
With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond
   ((and (buffer-narrowed-p) (not p))
    (widen))
   ((region-active-p)
    (narrow-to-region (region-beginning) (region-end)))
   ((derived-mode-p 'org-mode)
    ;; `org-edit-src-code' is not a real narrowing
    ;; command. Remove this first conditional if
    ;; you don't want it.
    (cond
     ((ignore-errors
        (org-edit-src-code)
        t)
      (delete-other-windows))
     ((ignore-errors
        (org-narrow-to-block)
        t))
     (t
      (org-narrow-to-subtree))))
   ((derived-mode-p 'latex-mode)
    (LaTeX-narrow-to-environment))
   (t
    (narrow-to-defun))))

(use-package
 selected
 :elpaca (selected :host github :repo "Kungsgeten/selected.el"))


(use-package
 phi-search
 :bind (("C-s" . phi-search) ("C-r" . phi-search-backward)))


;;;;;;;;;;
;; EVIL ;;
;;;;;;;;;;

(use-package
 evil
 :ensure t
 ;; :demand t
 :init
 ;; (setq evil-want-minibuffer t)
 ;; (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
 (setq evil-want-keybinding nil)
 :custom
 ;; (evil-want-integration t)
 (evil-undo-system 'undo-tree)
 :bind
 (("C-x e" . evil-mode)
  ;; :map evil-normal-state-map ("gc" . comment-dwim)
  :map evil-insert-state-map ("C-v" . yank)
  ;; ("C-e" . nil)
  ("C-p" . nil))
 :general
 ('(normal visual) "[ ]" 'evil-next-close-paren)
 ('(normal visual) "] [" 'evil-previous-open-paren)
 ('normal "j" 'evil-next-visual-line)
 ('normal "k" 'evil-previous-visual-line)
 ;; :hook (
 ;; )
 :config
 (define-key evil-motion-state-map [down-mouse-1] nil)
 (evil-mode 1))

;; Expands to: (elpaca evil (use-package evil :demand t))
;; (use-package
;;  evil
;;  :demand t
;;  :init (setq evil-want-keybinding nil)
;;  :hook (after-init . evil-mode)
;;  ;; :config (evil-mode)
;;  )


(use-package
 evil-collection
 :after evil
 :ensure t
 ;; :init
 ;; (setq evil-collection-magit-use-z-for-folds nil)
 ;; (evil-collection-init)
 ;; :config (evil-collection-init)
 :custom
 (evil-collection-elpaca-want-g-filters nil)
 (evil-collection-setup-minibuffer t)
 ;; ii(evil-collection-setup-minibuffer t "Add evil bindings to minibuffer")
 ;; (evil-collection-ement-want-auto-retro t)
 :custom (evil-collection-init))

(use-package
 evil-surround
 :elpaca t
 :after evil
 :config (global-evil-surround-mode 1))

(use-package
 evil-snipe
 :elpaca (evil-snipe :host github :repo "hlissner/evil-snipe")
 :after evil
 :demand
 :config
 (evil-snipe-mode +1)
 (evil-snipe-override-mode +1))

(use-package evil-nerd-commenter :ensure t)
;; visualize evil commands
(use-package
 evil-goggles
 :diminish evil-goggles-mode
 :after evil
 :defer 1
 :config (evil-goggles-mode)
 (setq
  evil-goggles-duration 0.8 ; show what I copied
  evil-goggles-async-duration 0.8 ; affects indenting
  evil-goggles-blocking-duration 0) ; don't want to wait when deleting
 (evil-goggles-use-diff-faces))


(use-package evil-anzu :diminish t :after (evil anzu))
;; (use-package evil-terminal-cursor-changer
;;   :elpaca (evil-terminal-cursor-changer :host github :repo "7696122/evil-terminal-cursor-changer")
;;   :config 
;;   (unless (display-graphic-p)
;;           (require 'evil-terminal-cursor-changer)
;;           (evil-terminal-cursor-changer-activate) ; or (etcc-on)
;;           ))
;; (use-package evil-cleverparens
;;   :elpaca (evil-cleverparens :host github :repo "emacs-evil/evil-cleverparens")
;;   :hook (emacs-lisp-mode . evil-cleverparens-mode))

(use-package anzu :diminish t :defer 10 :config (global-anzu-mode))


(message "loaded evil")

(use-package
 ace-window
 :bind ("M-o" . ace-window) ("C-c w" . ace-window)

 :config (setq aw-dispatch-always t)
 (setq
  aw-dispatch-alist
  '((?s aw-swap-window "Swap Windows")
    (?S aw-move-window "Move Window")
    (?c aw-copy-window "Copy Window")
    (?w aw-flip-window)
    (?b aw-switch-buffer-in-window "Select Buffer")
    (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
    (?k aw-delete-window "Delete Window")
    (?= aw-split-window-fair "Split Fair Window")
    (?- aw-split-window-vert "Split Vert Window")
    (?| aw-split-window-horz "Split Horz Window")
    (?m delete-other-windows "Delete Other Windows")
    (?? aw-show-dispatch-help))
  aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))

(use-package
 perspective
 :elpaca t
 :bind
 ("C-x C-b" . persp-list-buffers) ; or use a nicer switcher, see below
 :custom
 (persp-mode-prefix-key (kbd "C-c M-p")) ; pick your own prefix key here
 :init (persp-mode))


;;;;;;;;;;;;;;;;;;;
;; DOOM-MODELINE ;;
;;;;;;;;;;;;;;;;;;;

(use-package
 doom-modeline
 :ensure t
 :hook (after-init . doom-modeline-mode)
 :init
 (setq
  doom-modeline-icon t
  ;; doom-modeline-major-mode-icon t   ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  doom-modeline-modal-icon t
  doom-modeline-buffer-modification-icon t))

;; (elpaca-queue (elpaca '(melpulls :host github :repo "progfolio/melpulls")
;;                       (add-to-list 'elpaca-menu-functions #'melpulls)))

(use-package
 zzz-to-char
 :elpaca (zzz-to-char :host github :repo "mrkkrp/zzz-to-char")
 :bind ("C-c z" . zzz-to-char))

;;;;;;;;;;;;;;
;; SNIPPETS ;;
;;;;;;;;;;;;;;

;; (use-package yasnippet :elpaca t :init (yas-global-mode))


;; Configure Tempel
(use-package
 tempel
 :elpaca (tempel :host github :repo "minad/tempel")
 ;; Require trigger prefix before template name when completing.
 ;; :custom
 ;; (tempel-trigger-prefix "<")

 :bind
 (("M-+" . tempel-complete) ;; Alternative tempel-expand
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
               (cons #'tempel-expand completion-at-point-functions)))
 :hook (prog-mode . tempel-setup-capf) (text-mode . tempel-setup-capf)


 ;; Optionally make the Tempel templates available to Abbrev,
 ;; either locally or globally. `expand-abbrev' is bound to C-x '.
 ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
 ;; (global-tempel-abbrev-mode)
 )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package
 tempel-collection
 :elpaca
 (tempel-collection :host github :repo "Crandel/tempel-collection"))


;;;;;;;;;;;;;;;;
;; COMPLETION ;;
;;;;;;;;;;;;;;;;
(use-package
 cycle-at-point
 :general ('normal-state "M-z" 'cycle-at-point))

(use-package
 corfu
 :elpaca
 (corfu
  :host github
  :repo "minad/corfu"
  :files (:defaults "extensions/*"))
 :custom
 (corfu-auto t) ;; Enable auto completion
 (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
 (corfu-preselect 'prompt) ;; Always preselect the prompt
 (corfu-quit-at-boundary nil)
 (corfu-separator ?\s) ; Use space
 (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
 ;; (corfu-echo-documentation t)      ; Show documentation
 ;; (corfu-popupinfo-delay . 0.3)
 :init
 (global-corfu-mode)
 (corfu-history-mode t)
 (corfu-popupinfo-mode t)
 :bind
 (:map
  corfu-map
  ("TAB" . corfu-next)
  ([tab] . corfu-next)
  ("S-TAB" . corfu-previous)
  ([backtab] . corfu-previous))
 ;; :config
 ;; ;; (global-corfu-mode)
 ;; (with-eval-after-load 'evil
 ;;   (setq evil-complete-next-func (lambda (_) (completion-at-point))))
 )
(use-package
 corfu-terminal
 :elpaca
 (corfu-terminal
  :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
 ;; :ensure t
 :after corfu
 :config
 (unless (display-graphic-p)
   (corfu-terminal-mode +1)))


(use-package
 cape
 :general
 (:prefix
  "M-c" ; Choose a particular completion function
  "p"
  'completion-at-point
  "t"
  'complete-tag ; etags
  "d"
  'cape-dabbrev ; basically `dabbrev-completion'
  "f"
  'cape-file
  "k"
  'cape-keyword
  "s"
  'cape-symbol
  "a"
  'cape-abbrev
  "i"
  'cape-ispell
  "l"
  'cape-line
  "w"
  'cape-dict
  "\\"
  'cape-tex
  "_"
  'cape-tex
  "^"
  'cape-tex
  "&"
  'cape-sgml
  "r"
  'cape-rfc1345)
 :init
 (add-to-list 'completion-at-point-functions #'cape-dabbrev)
 (add-to-list 'completion-at-point-functions #'cape-file)
 (add-to-list 'completion-at-point-functions #'cape-elisp-block)
 (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package pcmpl-args :ensure t)


(use-package
 vertico
 :elpaca
 (vertico
  :type git
  :host github
  :repo "minad/vertico"
  :files (:defaults "extensions/*"))
 :custom (vertico-cycle t)
 ;; :hook (vertico-mode . turn-off-evil-mode)
 :init (vertico-mode) (setq vertico-resize t)
 :config (vertico-flat-mode t)
 :general
 (:keymaps
  'vertico-map "C-S" #'vertico-save
  "<escape>" #'vertico-exit ; Close minibuffer
  "<tab>" #'vertico-next "<backtab>" #'vertico-previous))

(use-package
 vertico-directory
 :elpaca nil
 :after vertico
 :general
 (vertico-map
  "RET"
  'vertico-directory-enter
  "DEL"
  'vertico-directory-delete-char
  "M-DEL"
  'vertico-directory-delete-word)
 ;; tidy shadowed file names
 :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;; repeat last vertico session
(use-package
 vertico-repeat
 :elpaca nil
 :after vertico
 :general ("M-r" 'vertico-repeat))


;; Optionally use the `orderless' completion style.
(use-package
 orderless
 :init
 (setq
  completion-styles '(orderless basic)
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))))
;; (use-package company
;;   :ensure t
;;   :hook (after-init . global-company-mode)
;;   :config (setq company-idle-delay 0
;; 		company-minimum-prefix-length 1)
;;   (company-tng-configure-default))


(use-package marginalia :ensure t :config (marginalia-mode))


(use-package
 embark
 :ensure t
 :bind
 (("C-." . embark-act) ;; pick some comfortable binding
  ("C-;" . embark-dwim) ;; good alternative: M-.
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
 (add-to-list
  'display-buffer-alist
  '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    nil
    (window-parameters (mode-line-format . none)))))

(use-package
 wgrep
 :elpaca (wgrep :repo github :repo "mhayashi1120/Emacs-wgrep"))

(use-package
 consult
 :elpaca ()
 :preface
 (defun consult-info-emacs ()
   "Search through Emacs info pages."
   (interactive)
   (consult-info "emacs" "efaq" "elisp" "cl" "compat"))
 (defun consult-info-org ()
   "Search through the Org info page."
   (interactive)
   (consult-info "org"))
 (defun consult-info-completion ()
   "Search through completion info pages."
   (interactive)
   (consult-info
    "vertico"
    "consult"
    "marginalia"
    "orderless"
    "embark"
    "corfu"
    "cape"
    "tempel"))
 :hook (completion-list-mode . consult-preview-at-point-mode)
 :bind
 (("C-M-#" . consult-register)
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info)
  ("C-x b" . consult-buffer)
  ("C-x p b" . consult-project-buffer)
  ("M-y" . consult-yank-pop)
  ("M-s d" . consult-find)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)))


;; Consult users will also want the embark-consult package.
(use-package
 embark-consult
 :ensure t ; only need to install it, embark loads it after consult if found
 :hook (embark-collect-mode . consult-preview-at-point-mode))


(use-package
 consult-project-extra
 :ensure t
 :bind
 (("C-c p f" . consult-project-extra-find)
  ("C-c p o" . consult-project-extra-find-other-window)))

(use-package flycheck :init (global-flycheck-mode))

;;;;;;;;;
;; VCS ;;
;;;;;;;;;
(use-package magit :ensure t :bind ("C-x g" . magit-status))

;; shows git information on fringe
(use-package
 diff-hl
 :hook (prog-mode . diff-hl-mode)
 ;; (dired-mode . diff-hl-dired-mode)
 ;; integration with magit
 (magit-pre-refresh . diff-hl-magit-pre-refresh)
 (magit-post-refresh . diff-hl-magit-post-refresh))


(use-package
 perspective-el
 :elpaca (perspective-el :host github :repo "nex3/perspective-el")
 :bind
 ("C-x C-b" . persp-list-buffers) ; or use a nicer switcher, see below
 :custom
 (persp-mode-prefix-key (kbd "C-c M-p")) ; pick your own prefix key here
 :init (persp-mode))

(use-package
 tree-sitter
 :elpaca t
 :defer t
 :config
 ;; (require 'tree-sitter-langs)
 (global-tree-sitter-mode)
 ;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
 :custom
 (setq treesit-language-source-alist
       '((cmake "https://github.com/uyha/tree-sitter-cmake")
         (css "https://github.com/tree-sitter/tree-sitter-css")
         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
         (json "https://github.com/tree-sitter/tree-sitter-json")
         (make "https://github.com/alemuller/tree-sitter-make"))))
;; (use-package tree-sitter-langs)
(use-package
 tree-sitter-langs
 :ensure t
 :hook
 ( ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  (c-mode-common . tree-sitter-hl-mode)
  (python-mode . tree-sitter-hl-mode)))

;; use tree sitter as evil text objects
(use-package
 evil-textobj-tree-sitter
 :disabled
 :elpaca
 (evil-textobj-tree-sitter
  :type git
  :host github
  :repo "meain/evil-textobj-tree-sitter"
  :files (:defaults "queries"))
 :after tree-sitter
 :config
 (define-key
  evil-outer-text-objects-map
  "s"
  (evil-textobj-tree-sitter-get-textobj "statement.outer"))
 (define-key
  evil-inner-text-objects-map
  "n"
  (evil-textobj-tree-sitter-get-textobj "scopename.inner"))
 (define-key
  evil-outer-text-objects-map
  "c"
  (evil-textobj-tree-sitter-get-textobj "call.outer"))
 (define-key
  evil-inner-text-objects-map
  "c"
  (evil-textobj-tree-sitter-get-textobj "call.inner"))
 (define-key
  evil-outer-text-objects-map
  "f"
  (evil-textobj-tree-sitter-get-textobj "function.outer"))
 (define-key
  evil-inner-text-objects-map
  "f"
  (evil-textobj-tree-sitter-get-textobj "function.inner")))


;;;;;;;;;;;;;;;;;
;; PROGRAMMING ;;
;;;;;;;;;;;;;;;;;

;;elisp
(use-package
 elisp-autofmt
 :elpaca
 (emacs-elisp-autofmt
  :type git
  :host codeberg
  :repo "ideasman42/emacs-elisp-autofmt")
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

;;cmake
(use-package
 cmake
 :elpaca nil
 :mode ("\\CMakeLists.txt\\'" . cmake-mode))

;;just files
(use-package
 justl
 :ensure t
 :bind (("C-c j" . justl-exec-recipe-in-dir))
 :custom (justl-executable "/usr/bin/just"))

(use-package just-mode :ensure t :defer t)

(use-package
 cmake-mode
 :ensure t
 :defer t
 :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
 :elpaca t)

;;rust
;; (use-package rust-mode :hook (rust-mode . rust-ts-mode))
(use-package
 rustic
 :elpaca t
 :hook (rustic-mode . (lambda () (flymake-mode -1)))
 :config
 (setq rustic-lsp-client 'eglot)
 (push 'rustic-clippy flycheck-checkers)
 (remove-hook 'rustic-mode-hook 'flycheck-mode))


;;haskell
(use-package haskell-mode :elpaca t :mode "\\.hs")

;;python
(use-package
 python
 :bind
 (:map
  python-ts-mode-map ("<f5>" . recompile)
  ;; ("<f6>" . eglot-format))
  )
 :hook ((python-ts-mode . eglot-ensure))
 :mode (("\\.py\\'" . python-ts-mode)))

;; Python virtual environment interface for Emacs
(use-package
 pyvenv
 :ensure t
 :defer t
 :hook (python-mode . pyvenv-mode))

;; edit .toml files
(use-package toml-mode :mode "\\.toml")

;; edit .yaml files
(use-package yaml-mode :mode "\\.yaml")

(use-package markdown-mode :mode "\\.yaml")


(use-package
 nushell-mode
 :elpaca
 (nushell-mode :type git :host github :repo "mrkkrp/nushell-mode"))

(use-package
 fish-mode
 :elpaca
 (fish-mode :type git :host github :repo "wwwjfy/emacs-fish"))

(use-package
 jai-mode
 :elpaca (jai-mode :type git :host github :repo "krig/jai-mode"))

(defun +aviik-pdf-viewer ()
  (advice-add
   #'TeX-command-master
   :before (lambda (&rest r) (save-buffer)))
  (push (list 'output-pdf "Zathura") TeX-view-program-selection))

(use-package
 auctex
 :ensure t
 :defer t
 :hook (LaTeX-mode . +aviik-pdf-viewer))

(use-package
 typst-ts-mode
 :elpaca
 (typst-ts-mode
  :type git
  :host sourcehut
  :repo "meow_king/typst-ts-mode")
 :custom (typst-ts-mode-watch-options "--open"))


;; (use-package
;;  highlight-indent-guides
;;  :ensure t
;;  :hook (python-ts-mode . highlight-indent-guides-mode)
;;  :config
;;  (set-face-foreground 'highlight-indent-guides-character-face "white")
;;  (setq highlight-indent-guides-method 'character))


(use-package
 eglot
 :elpaca nil
 :preface
 (defun kilo/eglot-capf ()
   (setq-local completion-at-point-functions
               (list
                (cape-super-capf
                 #'eglot-completion-at-point
                 #'tempel-expand
                 #'cape-file))))

 :custom-face (eglot-highlight-symbol-face ((t (:underline t :weight bold))))
 :hook (eglot-managed-mode . kilo/eglot-capf)
 :bind
 (:map
  eglot-mode-map
  ("C-c d" . eldoc)
  ("C-c a" . eglot-code-actions)
  ("C-c f" . flymake-show-buffer-diagnostics)
  ("C-c r" . eglot-rename))
 :config (add-to-list 'eglot-server-programs '(typst-mode . ("typst-lsp"))))

(use-package
 eldoc-box
 :elpaca (eldoc-box :host github :repo "casouri/eldoc-box")
 :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
 :after eglot)

(use-package
 consult-eglot
 :elpaca
 (consult-elgot :host github :repo "mohkale/consult-eglot")
 :after eglot
 :commands (consult-eglot-symbols))

(defalias 'kilo/eglot
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'eglot-code-actions)
    (define-key map (kbd "f") #'eglot-format)
    (define-key map (kbd "r") #'eglot-rename)
    map)
  "Eglot commands.")

(global-set-key (kbd "C-c c") 'kilo/eglot)


;;https://karthinks.com/software/jumping-directories-in-eshell/
(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
  (let ((eshell-dirs
         (delete-dups
          (mapcar
           'abbreviate-file-name
           (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell
              `(:name
                "Eshell"
                :narrow ?e
                :category file
                :face consult-file
                :items ,eshell-dirs))
             (consult-dir-sources
              (cons consult-dir--source-eshell consult-dir-sources)))
        (eshell/cd
         (substring-no-properties
          (consult-dir--pick "Switch directory: ")))))
     (t
      (eshell/cd
       (if regexp
           (eshell-find-previous-directory regexp)
         (completing-read "cd: " eshell-dirs)))))))

(use-package
 exec-path-from-shell
 :commands exec-path-from-shell-copy-env
 :config
 (when (memq window-system '(mac ns x))
   (exec-path-from-shell-initialize)))

(use-package vterm :ensure t)


;; (use-package emacs-coterm
;;   ;; :ensure t)
;;   :elpaca (emacs-coterm :type git
;; 			:repo "https://repo.or.cz/emacs-coterm.git"))


;; (message "loaded terminals")
;; ;; (use-package
;; (message "loaded doom-themes")
;; ;;  mistty
;;  :elpaca (mistty :host github :repo "szermatt/mistty")
;;  ;; :hook (mistty-mode . turn-off-evil-mode)
;;  :bind
;;  (("C-c x m" . mistty)

;;   ;; bind here the shortcuts you'd like the
;;   ;; shell to handle instead of Emacs.
;;   :map mistty-prompt-map

;;   ;; fish: directory history
;;   ("M-<up>" . mistty-send-key)
;;   ("M-<down>" . mistty-send-key)
;;   ("M-<left>" . mistty-send-key)
;;   ("M-<right>" . mistty-send-key)))


(use-package
 eat
 :elpaca
 (eat
  :type git
  :host codeberg
  :repo "akib/emacs-eat"
  :files
  ("*.el"
   ("term" "term/*.el")
   "*.texi"
   "*.ti"
   ("terminfo/e" "terminfo/e/*")
   ("terminfo/65" "terminfo/65/*")
   ("integration" "integration/*")
   (:exclude ".dir-locals.el" "*-tests.el")))
 :custom (eat-kill-buffer-on-exit t) (eat-eshell-mode t)
 :preface
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
 :bind
 (:map
  eat-semi-char-mode-map ("M-o" . crux-other-window-or-switch-buffer))
 (:map
  global-map
  ("<C-i> b" . jdm204-eat-bash)
  ("<C-i> n" . jdm204-eat-nu)
  ("C-`" . eshell)))


(use-package
 hl-todo
 :elpaca (hl-todo :type git :host github :repo "tarsius/hl-todo")
 :bind ("<C-c> i" . hl-todo-insert)
 :config (global-hl-todo-mode)
 ;; :hook (prog-mode . hl-todo-mode)
 ;; :custom
 ;; (setq hl-todo-keyword-faces
 ;;       '(("TODO" . "#FF0000")
 ;;         ("FIXME" . "#FF0000")
 ;;         ("DEBUG" . "#A020F0")
 ;;         ("GOTCHA" . "#FF4500")
 ;;         ("STUB" . "#1E90FF")))
 )
;; (use-package
;;  consult-todo
;;  :elpaca
;;  (consult-todo :host github :type git :repo "liuyinz/consult-todo")
;;  :demand t)

(elpaca-wait)
(use-package dired :elpaca nil)

(use-package
 org
 :elpaca nil
 :ensure nil
 ;; :pre-build (message "loading orgmode")
 :hook
 (org-mode . visual-line-mode)
 (org-mode . variable-pitch-mode)
 :config
 (setq org-directory
       (cond
        ((eq system-type 'gnu/linux)
         "~/Notes")
        ((eq system-type 'android)
         "/storage/emulated/0/Notes")
        (t
         "~/Notes")))
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
    " ┄┄┄┄┄ "
    "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  org-agenda-current-time-string "now ─────────────────────────────────────────────────")
 (setq
  org-startup-indented t
  org-pretty-entities t
  org-startup-with-inline-images t
  org-startup-folded t
  org-image-actual-width '(300))
 (electric-indent-mode -1)
 (setq org-edit-src-content-indentation 0)
 (require 'org-tempo)
 (org-babel-do-load-languages
  'org-babel-load-languages '((scheme . t) (emacs-lisp . t))))

(use-package
 toc-org
 :commands toc-org-enable
 :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package
 org-roam
 :after org
 :ensure t
 :custom (org-roam-directory (file-truename "~/OneDrive/org/OrgRoam"))
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
 (setq org-roam-node-display-template
       (concat
        "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
 (org-roam-db-autosync-mode))

;; tl;dr “<” to trigger org block completion at point.
(use-package
 org-block-capf
 :elpaca (:host github :repo "xenodium/org-block-capf"))

(load (expand-file-name "secrets.el" user-emacs-directory) 'noerror)

(use-package
 c3po
 :elpaca (c3po :host github :repo "d1egoaz/c3po.el")
 :config (setq c3po-api-key #'chatgpt-api-key))

(use-package
 password-generator
 :elpaca t
 :commands
 (password-generator-phonetic
  password-generator-strong password-generator-paranoid))

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

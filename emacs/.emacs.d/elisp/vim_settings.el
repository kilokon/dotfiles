;; Evil

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil
        evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))


(when (require 'evil-collection nil t)
  (evil-collection-init))

(setq evil-search-wrap t
      evil-regexp-search t)

;; evil-commentary
(require 'evil-commentary)
(evil-commentary-mode)


;; General
(require 'general)

;; Global Keybindings
;; * Mode Keybindings
(general-define-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 ;; or xref equivalent
 "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
;; `general-def' can be used instead for `evil-define-key'-like syntax
(general-def 'normal emacs-lisp-mode-map
  "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
;; again, variables are not necessary and likely not useful if you are only
;; using a definer created with `general-create-definer' for the prefixes
;; (defconst my-leader "SPC")
;; (defconst my-local-leader "SPC m")

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC m")

;; ** Global Keybindings
(my-leader-def
  :keymaps 'normal
  ;; bind "SPC a"
  "a" 'org-agenda
  "b" 'counsel-bookmark
  "c" 'org-capture)
;; `general-create-definer' creates wrappers around `general-def', so
;; `evil-global-set-key'-like syntax is also supported
(my-leader-def 'normal
  "a" 'org-agenda
  "b" 'counsel-bookmark
  "c" 'org-capture)

;; to prevent your leader keybindings from ever being overridden (e.g. an evil
;; package may bind "SPC"), use :keymaps 'override
(my-leader-def
  :states 'normal
  :keymaps 'override
  "a" 'org-agenda)
;; or
(my-leader-def 'normal 'override
  "a" 'org-agenda)

;; ** Mode Keybindings
(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "y" 'org-store-link
  "p" 'org-insert-link
  ;; ...
  )
;; `general-create-definer' creates wrappers around `general-def', so
;; `evil-define-key'-like syntax is also supported
(my-local-leader-def 'normal org-mode-map
  "y" 'org-store-link
  "p" 'org-insert-link
  ;; ...
  )
(defconst kilo-key "<Space>") 
(general-create-definer kilo-leader-key
  :prefix kilo-key)

;; (evil-define-key 'normal 'global
  ;; select the previously pasted text
  ;; "qq" (kbd ":wq")
  ;; run the macro in the q register
  ;; "Q" "@q"
  ;; )
;; * Settings
;; change evil's search module after evil has been loaded (`setq' will not work)


(general-setq evil-search-module 'evil-search)


(setq evil-default-state 'emacs
      evil-emacs-state-modes nil
      evil-insert-state-modes nil
      evil-motion-state-modes nil
      evil-normal-state-modes '(text-mode prog-mode fundamental-mode
                                          css-mode conf-mode
                                          TeX-mode LaTeX-mode
                                          diff-mode))


;;Which-Key

(require 'which-key)
(which-key-mode)
;;(general-evil-setup)
;; * Global Keybindings
;; all keywords arguments are still supported
;; these are just wrappers around `general-def' that set a default :states
;;(general-nmap
;;  :prefix "SPC"
;;  "p" 'helm-mini)

;; bind in motion state (inherited by the normal, visual, and operator states)
;;(general-mmap
;;  ";" 'evil-ex
;;  ":" 'evil-repeat-find-char)

;; alternatively, for shorter names
(general-evil-setup t)
;;(mmap
;;  ";" 'evil-ex
;;  ":" 'evil-repeat-find-char)

;; * Mode Keybindings
(general-nmap
  :keymaps 'emacs-lisp-mode-map
  "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
;; same as
(general-nmap emacs-lisp-mode-map
  "K" 'elisp-slime-nav-describe-elisp-thing-at-point)


(defvar evil-collection--supported-modes
  `(
    ;; ...
    eldoc
    ;; ...
    )
  "List of modes supported by evil-collection. Elements are
either target mode symbols or lists which `car' is the mode
symbol and `cdr' the packages to register.")



(provide 'vim_settings)

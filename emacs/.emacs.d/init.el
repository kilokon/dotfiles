;;--------------
;; ┏━┓╋┏┳━━━┳━┓┏┳━━━┳━━━┳━━┳━┓┏┓
;; ┃┃┃╋┃┃┏━┓┣┓┃┃┃┏━┓┃┏━┓┣┫┣┻┓┃┃┃
;; ┃┃┗┓┃┃┃╋┃┃┃┗┛┃┃╋┃┣┛╋┃┃┃┃╋┃┗┛┃
;; ┃┏┓┗┛┃┃╋┃┃┏┏┓┃┃╋┃┃╋╋┃┃┃┃╋┏┏┓┃
;; ┃┃┗┓┃┃┗━┛┣┛┃┃┃┗━┛┃╋╋┃┣┫┣┳┛┃┃┃
;; ┗┛╋┗━┻━━━┻━┛┗┻━━━┛╋╋┗┻━━┻━┛┗┛
;;+-------------------------+
;;|   Early Configuration   |
;;+-------------------------+
;;(setq gc-cons-threshold most-positive-fixnum) ; avoid GC during startup to save time

(defalias 'yes-or-no-p 'y-or-n-p)


;;PERSONAL INFO
;;-------------

;;Personal information
(setq user-full-name "kilokon"
      user-mail-address "kilo.kon@outlook.com")

;;+---------------+
;;|   Defconsts   |
;;+---------------+
;;I keep pressing :wq
(defconst wq "Use C-x C-c")
(defvar kK/backup-dir (expand-file-name "backups" user-emacs-directory))

;;+------------+
;;|   Defuns   |
;;+------------+
;;----------
(defun aviik/reload-init-file()
 "It reloads Emacs init config."
 (interactive)
 (load-file user-init-file))

(defun aviik/switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

;;;;http://chopmo.dk/2016/10/27/emacs-highlighting-current-word.html
;;;;(require 'hi-lock)
(defun aviik/toggle-mark-word-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))

;;https://www.emacswiki.org/emacs/CopyingWholeLines
(defun aviik/quick-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
Consecutive calls to this command append each line to the
kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

;;https://github.com/magnars/.emacs.d/blob/master/defuns/file-defuns.el
(defun aviik/copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new (if eproject-mode
                  (s-chop-prefix (eproject-root) filename)
                filename))))


;; hitting <C-S-backspace>, the binding for kill-whole-line appends to kill ring


(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key
(current-word)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)
;;Available Servers
;;-----------------

;;+--------------------+
;;|   Elisp Learning   |
;;+--------------------+
;; Learnin @ Xah Lee
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (message "Microsoft Windows")))
 ((string-equal system-type "darwin") ;  macOS
  (progn
    (message "Mac OS X")))
 ((string-equal system-type "gnu/linux")
  (progn
    (message "Linux"))))

;; Classic if-else
(if (version< emacs-version "27.1")
    (message "too old")
  (message "good"))

;; If else with seuential evaluation
(if (< emacs-major-version 28)
    (progn
      (message "not emacs 28"))
  (progn
    (message "yes emacs 28 or later")))

;; progn - evaluate in sequence 
(if (display-graphic-p)
    ;; GUI mode
    (progn
      (message "I'm in Gui mode!")
      (message "Loading Graphic-Mode Icons and Themes"))
    ;; Terminal mode
    (message "I'm in terminal mode")
    (message "Unable to load graphic mode setup"))


;;+--------------------+
;;|  File Management   |
;;+--------------------+


(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
;; Backup File
(unless (file-exists-p kK/backup-dir)
  (mkdir kK/backup-dir))


(setq
 create-lockfiles nil        ;; Don't create lock files.
 backup-by-copying t      ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t       ; use versioned backups
 backup-directory-alist
 `((".*" . ,kK/backup-dir))
 auto-save-file-name-transforms
 `((".*" ,kK/backup-dir t)))



;https://www.emacswiki.org/emacs/backup-each-save.el
;;(load "~/.emacs.d/scripts")
(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;;History
(savehist-mode)
;;Recent Files
(recentf-mode t)

;;+---------------------+
;;|   sexp Selection    |
;;+---------------------+
(delete-selection-mode 1)           ;; Replace region when inserting text
(global-subword-mode 1)             ;; Iterate through CamelCase words



;;Default Settings
;;----------------
(setq visible-bell 1                    ; Annoying Ding off
      ring-bell-function 'ignore
      inhibit-startup-screen t
      initial-major-mode 'org-mode
      initial-scratch-message ""
      debug-on-error t                  ; Debug
      edebug-all-forms t                ; Debug
      undo-limit 200000                 ; Undo Limits
      undo-strong-limit 40000000        ; Undo Limits
      load-prefer-newer t               ; Load Newer el files
      make-pointer-invisible t          ; Hide Mouse While editing
      package-enable-at-startup nil     ; Disable package.el in favor of straight.el
      enable-recursive-minibuffers t    ; Enable recursive minibuffers
      tab-always-indent 'complete       ; Enable indentation+completion using the TAB key.
      completion-cycle-threshold 3      ; TAB cycle if there are only few candidates
      )

;;Line Numbering ;; Disable line numbers for some modes
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		vundo-1
                term-mode-hook
		vterm-mode-hook
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
(menu-bar-mode -1)
(set-fringe-mode 7)
(global-hl-line-mode 1)
;;+-----------+
;;|   Theme   |
;;+-----------+
(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "doomemacs/themes")
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;+------------+
;;|   Cursor   |
;;+------------+
(blink-cursor-mode 0)
(setq-default cursor-type 'box)

(use-package multiple-cursors
  :straight (multiple-cursors :type git :host github :repo "magnars/multiple-cursors.el")
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" .  mc/mark-all-like-this)))

;;+---------------------------+
;;|   UI: Frames and Window   |
;;+---------------------------+

(use-package burly
  :straight (burly :type git
		   :host github
		   :repo "alphapapa/burly.el"))



;;https://gist.github.com/rksm/8c07d9ccc9e15adf752d3dd73dd9a61e
(defun rk/open-compilation-buffer (&optional buffer-or-name shackle-alist shackle-plist)
  "Helper for selecting window for opening *compilation* buffers."
  ;; find existing compilation window left of the current window or left-most window
  (let ((win (or (loop for win = (if win (window-left win) (get-buffer-window))
                       when (or (not (window-left win))
                                (string-prefix-p "*compilation" (buffer-name (window-buffer win))))
                       return win)
                 (get-buffer-window))))
    ;; if the window is dedicated to a non-compilation buffer, use the current one instead
    (when (window-dedicated-p win)
      (let ((buf-name (buffer-name (window-buffer win))))
        (unless (string-prefix-p "*compilation" buf-name)
          (setq win (get-buffer-window)))))
    (set-window-buffer win (get-buffer buffer-or-name))
    (set-frame-selected-window (window-frame win) win)))


(use-package shackle
  :straight (shackle :type git
		     :host nil
		     :repo "https://depp.brause.cc/shackle.git")
  :diminish t
  :custom
  ((shackle-rules
    (let ((repls "\\*\\ielm\\slime\\"))
     '((compilation-mode :custom rk/open-compilation-buffer :select t)
       ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
       ("\\*magit" :regexp t :same t :select t)
;;       ("\\*vterm*" :regexp t :same nil :select t :size 0.75)
;;       ("\\*PowerShell.*" :regexp t :same t :select t)
;;       ("\\*Cargo.*" :regexp t :other t :select nil)
       ("*Messages*" :select nil :other t)
;;       ("*Proced*" :select t :same t)
       ("*Buffer List*" :select t :same t)
;;       ("\\*Pp Eval" :regexp t :same nil :select t :other t) 
;;       ("*Messages*" :same nil :other t :select t :inhibit-window-quit t)
       ("*Help*" :other t :select nil :same nil :align bottom :size 0.5)
		   ;; slime
       ("*slime-source*" :select nil :same nil :other t)
       ("*slime-description*" :select nil :other t :inhibit-window-quit t)
       ("\\*slime-repl" :regexp t :same nil :select nil :other t)
		   ;; ("\\*sldb" :regexp t :other t :inhibit-window-quit t :select t)
       ("\\*slime-compilation" :regexp t :same nil :select nil :other t)
       ("*slime-scratch*" :same nil :select t :other t))))
   (shackle-default-rule nil))
  :config (shackle-mode))
;;+---------------------------+
;;|   Ui: Buffer Management   |
;;+---------------------------+

;; (use-package iflipb
;;   :straight (iflipb :type git :host github :repo "jrosdahl/iflipb")
;;   :bind (("C-<VoidSymbol" . iflipb-next-buffer)))


;;+-------------------------------+
;;|   UI: Font Settings: Global   |
;;+-------------------------------+
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :weight 'light :height 100)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :weight 'light :height 120)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.1)



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
   (("C-M-;" . magit-status)
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
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


;;+-----------------+
;;|   Undo & Redo   |
;;+-----------------+
(straight-use-package 'undo-fu)

(use-package vundo 
  :straight (vundo :type git :host github :repo "casouri/vundo")
  :bind ("C-M-'" . vundo)
  :config (set-face-attribute 'vundo-default nil :family "Symbola"))

;; Vundo (visual undo) displays the undo history as a tree and lets you
;; move in the tree to go back to previous buffer states. To use vundo,
;; type M-x vundo RET in the buffer you want to undo. An undo tree buffer
;; should pop up. To move around, type:

;;   f   to go forward
;;   b   to go backward

;;   n   to go to the node below when you at a branching point
;;   p   to go to the node above

;;   a   to go back to the last branching point
;;   e   to go forward to the end/tip of the branch

;;   q   to quit, you can also type C-g




;;+------------------+
;;|   Key Controls   |
;;+------------------+
;;Unset a few keys
(-map (lambda (x) (unbind-key x)) '("C-x C-d" ;; list-directory
                                    "C-z" ;; suspend-frame
                                    "C-x C-z" ;; again
                                    "M-o" ;; facemenu-mode
				    "C-a"
                                    ;; "<mouse-2>" ;; pasting with mouse-wheel click
                                    ;; "<C-wheel-down>" ;; text scale adjust
                                    ;; "<C-wheel-up>" ;; ditto
                                    "s-n" ;; make-frame
                                    "C-x C-q" ;; read-only-mode
				    "M-m"
                                    ))

(use-package which-key
  :straight (which-key :type git :host github :repo "justbur/emacs-which-key")
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))



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
                                  replace)))
(general-define-key
:states '(insert emacs normal hybrid motion visual operator)
 :prefix "SPC"
 :non-normal-prefix "S-SPC"
 :keymaps 'override)

;;https://github.com/mohkale/spaceleader
(defconst leader-minor-mode-leader-prefix "q"
  "leader key for minor mode bindings.
this leader prefix is expected to be muddled and unreliable...
due to tonnes of different minor modes collabratively binding to it.

That said... I need a place to put minor-mode keys and this was
unoccupied.")

(defconst leader-server-leader-prefix "l"
  "put leader keys related to active servers under this prefix.")

(defconst leader-diff-leader-prefix "d"
  "leader prefix under which diff bindings are assigned.")


(use-package bind-map
  :straight t)




;; Settings come from
;;https://github.com/mohkale/emacs/blob/master/init.org#leader
(use-package spaceleader
  :straight (spaceleader :type git :host github :repo "mohkale/spaceleader")
  :demand t 
  :config
 (require 'spaceleader-use-package)
 (leader-declare-prefix leader-server-leader-prefix "lang-server")
 (leader-declare-prefix leader-minor-mode-leader-prefix "minor-modes")

  :general
  ("C-@" (general-simulate-key "C-SPC")) ;; C-SPC in terminal
  ;; Make my none-normal leader key active even in normal states.
  (:states leader-norm-states
   "C-SPC" (eval `(general-simulate-key ,leader-key)))
  ;; Setup C-, to trigger my major-mode leader-keys in both insert and normal states.
  (:keymaps 'override
   :states leader-norm-states
   "C-," (eval `(general-simulate-key ,(concat leader-key " " leader-major-mode-prefix))))
  (:keymaps 'override
   :states leader-nnorm-states
   "C-," (eval `(general-simulate-key ,(concat leader-nnorm-key " " leader-major-mode-prefix)))))


(leader-set-keys
  "TAB" '(switch-to-last-buffer+ :wk "last-buffer")
  "SPC" '(execute-extended-command-for-buffer :wk "M-x"))






;; remembering my day to day shortcuts
;;
;; Esc followed by < #beginning of file
;; Esc followed by > #end of file
;; C-l clears terminal 



;;Global-Shortcuts
(global-set-key (kbd "C-c a r") 'aviik/reload-init-file)
(global-set-key (kbd "C-c a s") 'aviik/switch-to-scratch-buffer)
(global-set-key (kbd "C-c a r") 'recentf-open-files)
(global-set-key (kbd "C-c a t") 'load-theme)
(global-set-key (kbd "C-z") 'undo-fu)
;;(global-set-key (kbd "M-<up>") 'mode-line-other-buffer)
(global-set-key (kbd "C-<VoidSymbol>") 'other-window)
(global-set-key (kbd "s-.") 'aviik/toggle-mark-word-at-point)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "C-c c c") 'aviik/quick-copy-line)
(global-set-key (kbd "C-c C-w") 'kill-buffer-and-window)


;; (leader/declare-prefix
;;   "a"  '("apps" . "applications"))




;; (leader/set-keys
;; ;;  "au" 'undo-tree-visualise
;; ;;  "ax" 'customize

;;   "amw" 'load-theme
;;   "amm" 'recentf-open-files)
;;+-----------------------+
;;|   Vi Style Workflow   |
;;+-----------------------+

;;;Vim coltrol avoid emacs navigation getting slower

;; (use-package evil
;;   :straight (evil :type git
;; 		  :host github
;; 		  :repo "emacs-evil/evil")
;;   :init (setq evil-disable-insert-state-bindings t
;; 	      evil-want-integration t
;; 	      evil-respect-visual-line-mode t
;; 	      evil-want-keybinding nil
;; 	      evil-undo-system 'undo-fu)
;;   :config (evil-mode))

;; (use-package evil-collection
;;   :straight (evil-collection :type git
;; 			     :host github
;; 			     :repo "emacs-evil/evil-collection")
;;   :after evil
;;   :init (evil-collection-init)
;;   :custom (evil-collection-setup-minibuffer t))

;; (use-package evil-nerd-commenter
;;   :straight (evil-nerd-commenter :type git
;;                                  :host github
;;                                  :repo "redguardtoo/evil-nerd-commenter")
;;   :bind ("M-;" . evilnc-comment-or-uncomment-lines))


;;+----------------------------+
;;|   Emacs Interface to w3m   |
;;+----------------------------+
(use-package w3m
  :straight (emacs-w3m :type git
		       :host github
		       :repo "emacs-w3m/emacs-w3m")
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
  :straight (all-the-icons :type git
			   :host github
			   :repo "domtronn/all-the-icons.el"))


;;+---------------------+
;;|       Dired         |
;;+---------------------+
;;(straight-use-package '(<package-name> :local-repo "~/.emacs.d/lisp/<package-name>" :type nil))
(use-package dwim-shell-command
  :straight (dwim-shell-command :local-repo "~/.cache/emacs/scripts/dwim-shell-command"
				:type nil)
;;  :message ("loaded dwim-shell-command")
  :bind
  ("M-!" . dwim-shell-command)
  :config (message "dwim-shell-command is loaded"))

(use-package dired
  :straight (:type built-in)
  :init
   (setq dired-omit-files "^\\.[^.]\\|$Rhistory\\|$RData\\|__pycache__"
         dired-listing-switches "-laht"
	 dired-kill-when-opening-new-dired-buffer t
	 ;; ls-lisp-use-insert-directory-program nil
	 dired-dwim-target t)
   :bind (:map dired-mode-map
              ([remap dired-do-async-shell-command] . dwim-shell-command)
              ([remap dired-do-shell-command] . dwim-shell-command)
              ([remap dired-smart-shell-command] . dwim-shell-command)))

(use-package dired-x
  :straight (:type built-in)
  :after dired)

(use-package dired-single
  :straight (dired-single :type git
			  :host github
			  :repo "crocket/dired-single")
  :after dired
  :general
  (dired-mode-map
   :states 'normal
   "h" 'dired-single-up-directory
   "l" 'dired-single-buffer
   "q" 'kill-current-buffer))

(use-package all-the-icons-dired
  :straight (all-the-icons-dired :type git
				 :host github
				 :repo "jtbm37/all-the-icons-dired")
  :if (display-graphic-p)
  :hook (dired-mode . (lambda () (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))))



(use-package zoxide
  :straight (zoxide :type git
		    :host gitlab
		    :repo "Vonfry/zoxide.el"))
  ;; :general (aviik/leadr-key-def
  ;; 	     "gz" 'zoxide-find-file))

(defun dired-jump-with-zoxide (&optional other-window)
   (interactive "P")
   (zoxide-open-with nil (lambda (file) (dired-jump other-window file)) t))


;; https://www.reddit.com/r/emacs/comments/vwbf6f/applying_snippets_in_different_languages_to_dired/
(defun dwim-shell-command-csv-to-json-via-python ()
  "Convert csv file to json (via Python)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert csv file to json (via Python)."
   "
import csv
import json
text = json.dumps({ \"values\": list(csv.reader(open('<<f>>')))})
fpath = '<<fne>>.json'
with open(fpath , 'w') as f:
  f.write(text)"
   :shell-util "python"
   :shell-args "-c"))


(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  :bind
  (("C-c ," . embark-act)       ;; pick some comfortable binding
   ("C-c ." . embark-dwim)
   ("C-s-e" . embark-export)
   ("C-h b" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-action-indicator
	(lambda (map &optional _target)
	  (which-key--show-keymap "Embark" map nil nil 'no-paging)
	  #'which-key--hide-popup-ignore-command)
	embark-become-indicator embark-action-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

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
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
                  ("M-h" . backward-kill-word))
;;  :general
;;  (aviik/leadr-key-def
;;    "ff" 'find-file )
  :hook (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle t) 
  :init
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

;;(define-key global-map [remap execute-extended-command] #'completion vertico)
;;(define-key global-map [remap find-file] #'vertico)

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

(setq enable-recursive-minibuffers t)



(use-package consult
  :straight (consult :type git :host github :repo "minad/consult")
  :bind (("C-s" . consult-line))
  :custom (completion-in-region-function #'consult-completion-in-region))

(use-package consult-dir
  :straight (consult-dir :type git :host github :repo "karthink/consult-dir")
  :init (setq register-preview-delay 0.1
	      register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
	  xref-show-definitions-function #'consult-xref)
  :bind (("C-x b" . consult-buffer)
	 ("C-c b" . consult-bookmark)
	 ("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package orderless
  :straight (orderless :type git :host github :repo "oantolin/orderless")
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
	   (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
	  (cons (cdr x) (substring pattern 1))
	(when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
	  (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	  ;;; Enable partial-completion for files.
	  ;;; Either give orderless precedence or partial-completion.
	  ;;; Note that completion-category-overrides is not really an override,
	  ;;; but rather prepended to the default completion-styles.
	;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
	completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
					;; enable initialism by default for symbols
					(command (styles +orderless-with-initialism))
					(variable (styles +orderless-with-initialism))
					(symbol (styles +orderless-with-initialism)))
	orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
	orderless-style-dispatchers '(+orderless-dispatch)))







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





;;+----------------------+
;;|  Snippet Management  |
;;+----------------------+
;;Yasnippet
(use-package yasnippet
  :straight (yasnippet :type git
		       :host github
		       :repo "joaotavora/yasnippet")
  ;;:after smart-tab
  :custom (
	   setq yas-snippet-dirs
		'("~/.cache/emacs/snippets"))
  ;; :general
  ;; ("C-;" 'yas-insert-snippet)
  :config
  (use-package yasnippet-snippets
    :straight (yasnippet-snippets :type git
				  :host github
				  :repo "AndreaCrotti/yasnippet-snippets"))
  (yas-global-mode 1))


(use-package consult-yasnippet
  :straight (consult-yasnippet :type git
			       :host github
			       :repo "mohkale/consult-yasnippet")
;;  :after (consult yasnippet)
  :bind ("C-;" . consult-yasnippet))

;;+-------------------------+
;;|   Completion System     |
;;+-------------------------+
;;https://codeberg.org/takeonrules/dotemacs/src/branch/main/emacs.d/configuration.org
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  :demand t
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
	      ("<escape>". corfu-quit)
	      ("<return>" . corfu-insert)
	      ("M-d" . corfu-show-documentation)
	      ("M-l" . 'corfu-show-location)
	      ("TAB" . corfu-next)
	      ([tab] . corfu-next)
	      ("S-TAB" . corfu-previous)
	      ([backtab] . corfu-previous))
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu
  
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  
  ;; (corfu-min-width 80)
  ;; (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  
  ;; (corfu-echo-documentation nil)        ; Already use corfu-doc
  (corfu-separator ?\s)                 ; Necessary for use with orderless
  (corfu-quit-no-match 'separator)
  
  (corfu-preview-current 'insert)       ; Preview current candidate?
  (corfu-preselect-first t)             ; Preselect first candidate?
  
  :init (global-corfu-mode))




(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  ;; (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))




(use-package corfu-doc
  ;; NOTE 2022-02-05: At the time of writing, `corfu-doc' is not yet on melpa
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :bind (:map corfu-map
	      ;; This is a manual toggle for the documentation window.
	      ([remap corfu-show-documentation] . corfu-doc-toggle) ; Remap the default doc command
	      ;; Scroll in the documentation window
	      ("M-n" . corfu-doc-scroll-up)
	      ("M-p" . corfu-doc-scroll-down))
  :hook (corfu-mode . corfu-doc-mode)
  :custom
  (corfu-doc-delay 0.1)
  (corfu-doc-hide-threshold 10)
  (corfu-doc-max-width 60)
  (corfu-doc-max-height 10)

  ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
  ;; extra-safe that this is set when corfu-doc is loaded. I do not want
  ;; documentation shown in both the echo area and in the `corfu-doc' popup.
  ;; (corfu-echo-documentation nil)
  :config
  ;; NOTE 2022-02-05: This is optional. Enabling the mode means that every corfu
  ;; popup will have corfu-doc already enabled. This isn't desirable for me
  ;; since (i) most of the time I do not need to see the documentation and (ii)
  ;; when scrolling through many candidates, corfu-doc makes the corfu popup
  ;; considerably laggy when there are many candidates. Instead, I rely on
  ;; manual toggling via `corfu-doc-toggle'.
  (corfu-doc-mode))




(use-package cape
  :straight (cape :type git :host github :repo "minad/cape")
  :bind (("C-c p p" . completion-at-point)
	 ("C-c p d" . cape-dabbrev)
	 ("C-c p f" . cape-file)
	 ("C-c p s" . cape-symbol)
	 ("C-c p i" . cape-ispell)))

  ;; Use Company backends as Capfs.
  (setq-local completion-at-point-functions
    (mapcar #'cape-company-to-capf
	    (list #'company-files #'company-ispell #'company-dabbrev)))







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
  ;;:init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;;(setq lsp-keymap-prefix "C-c l")
  :hook (((c-mode
	   c++-mode
	   python-mode
	   rustic-mode
	   haskell-mode) . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-ui-mode))
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
    ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  ;;:config
  ;;(setq rustic-lsp-server 'rust-analyzer)
  :bind (:map lsp-mode-map
	      ("TAB" . completion-at-point)
              ("C-c l d" . xref-find-definitions)
              ("C-c l r" . xref-find-references)
              ("C-c l n" . lsp-ui-find-next-reference)
              ("C-c l p" . lsp-ui-find-prev-reference)
              ("C-c l i" . counsel-imenu)
	      ("C-c l e" . lsp-ui-flycheck-list)
              ("C-c l q" . lsp-workspace-restart)
              ("C-c l Q" . lsp-workspace-shutdown)
	      ("C-c l S" . lsp-ui-sideline-mode)
              ("C-c l X" . lsp-execute-code-action)))
             ;; ("C-c l n"   . lsp-ui-peek-find-implementation)))

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
;;  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-code-actions-prefix ""))



(use-package consult-lsp
  :straight (consult-lsp :type git :host github :repo "gagbo/consult-lsp")
  :bind(("C-c E" . consult-lsp-diagnostics)
	("C-c S" . consult-lsp-symbols)
	("C-c F" . consult-lsp-file-symbols)))




;; (use-package company
;;   :straight (company :type git
;;                      :host github
;;                      :repo "company-mode/company-mode")
;;   :diminish (company-mode  .  " Ⓒ ")
;;   :bind (:map company-active-map
;;          ("C-n" . company-select-next)
;;          ("C-p" . company-select-previous)
;; 	(:map company-mode-map
;; 	("<tab>". tab-indent-or-complete)
;; 	("TAB". tab-indent-or-complete)))
;;   :init
;;   (global-company-mode)
;;   :config (setq company-backends
;; 		'((
;; ;;		   company-capf
;; ;;		   company-fish-shell
;; 		   company-dabbrev-code)
;; 		  (company-abbrev company-dabbrev)))
  
  ;; (use-package company-statistics
  ;;   :straight t
  ;;   :init
  ;;   (company-statistics-mode))

  ;; (use-package company-web
  ;;   :straight t)

  ;; (use-package company-try-hard
  ;;   :straight t
  ;;   :bind
  ;;   (("C-<tab>" . company-try-hard)
  ;;    :map company-active-map
  ;;    ("C-<tab>" . company-try-hard)))

;;   (use-package company-quickhelp
;;     :straight t
;;     :config
;;     (company-quickhelp-mode)))


;; (use-package company-box
;;   :straight (company-box :type git
;; 			 :host github
;; 			 :repo "sebastiencs/company-box")
;;   :hook (company-mode . company-box-mode))


;; (use-package company-ghci
;;   :straight (company-ghci :type git
;; 			  :host github
;; 			  :repo "horellana/company-ghci"))

;; (use-package company-shell
;;   :straight (company-shell :type git
;; 			   :host github
;; 			   :repo "Alexander-Miller/company-shell"))


;;+-----------------------------------------+
;;|   Garbage Collection & Parser Priming   |
;;+-----------------------------------------+

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

;;+-----------------------------------+
;;|   Terminals & External Payloads   |
;;+-----------------------------------+


(use-package eshell
  :straight (:type built-in)
;;  :commands (aviik/toggle-eshell)
  :custom (setq eshell-aliases-file (expand-file-name "/eshell/aliases" user-emacs-directory)
		eshell-directory-name "~/.dotfiles")
  ;; :general
  ;; (aviik/leadr-key-def
  ;;  "e" '(:ignore t :which-key "Eshell")
  ;;  "ee" #'aviik/toggle-eshell)
  :config
  (setq eshell-cmpl-ignore-case t
	eshell-cd-on-directory t
	eshell-prompt-function (lambda nil
				 (concat
				  (propertize (format "[%s]" (abbreviate-file-name (eshell/pwd))) 'face '(:foreground "#8787af"))
				  ;; (propertize " λ " 'face '(:foreground "#f75f5f"))
				  (propertize " ॐ " 'face `(:foreground "white"))
				  (propertize " ❯ " 'face '(:foreground "#f75f5f"))))
	eshell-prompt-regexp "^λ "
	eshell-history-size         50
	eshell-buffer-maximum-lines 300
	eshell-hist-ignoredups t
	eshell-highlight-prompt t
	eshell-save-history-on-exit t
        eshell-destroy-buffer-when-process-dies t
	eshell-scroll-to-bottom-on-input t
	eshell-prefer-lisp-functions nil))


(add-hook 'eshell-mode-hook (lambda ()
			      (eshell/alias "ff" "find-file $1")
			      (eshell/alias "ee" "find-file-other-window $1")
			      (eshell/alias "gd" "magit-diff-unstaged")
			      (eshell/alias "gds" "magit-diff-staged")
			      (eshell/alias "d" "dired $1")
;;			      (eshell/alias "cdO" (concat cd " ~/OneDrive"))
			      (eshell/alias "ll" "ls -la")))


;;;https://karthinks.com/software/jumping-directories-in-eshell/
(defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
    (let ((eshell-dirs (delete-dups
                        (mapcar 'abbreviate-file-name
                                (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                             :narrow ?e
                                             :category file
                                             :face consult-file
                                             :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell
                                          consult-dir-sources)))
          (eshell/cd (substring-no-properties
                      (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                            (completing-read "cd: " eshell-dirs)))))))

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;Path to Shell
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :type git
				  :host github
				  :repo "purcell/exec-path-from-shell")
  :init (exec-path-from-shell-initialize))

(defalias 'eshell/v 'eshell-exec-visual)


;; (use-package shell-pop
;;   :straight (shell-pop :type git :host github :repo "kyagi/shell-pop-el")
;;   :bind (("C-c s b" . pop-bash))
;;   :config (
;; 	   (defun pop-bash ()
;; 	     (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
;; 	     (shell-pop-term-shell "/bin/bash")
;; 	     (shell-pop-window-size 30)
;; 	     (shell-pop-full-span t)
;; 	     (shell-pop-window-position "bottom")
;; 	     (shell-pop-autocd-to-working-dir t)
;; 	     (shell-pop-restore-window-configuration t)
;; 	     (shell-pop-cleanup-buffer-at-process-exit t))))






;; (use-package eshell-toggle
;;   :straight (eshell-toggle :type git
;; 			   :host github
;; 			   :repo "4DA/eshell-toggle")
;;   :custom
;;   (eshell-toggle-size-fraction 3)
;;   (eshell-toggle-use-projectile-root nil)
;;   ;; (eshell-toggle-use-projectile-root t)
;;   (eshell-toggle-run-command nil)
;;   ;;(eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
;;   :bind
;;   ("s-`" . eshell-toggle))








(use-package vterm
  :straight (vterm :type git
		   :host github
		   :repo "akermu/emacs-libvterm")
  :commands (vterm vterm-other-window)
  :bind (("C-c v" . vterm)
         :map vterm-mode-map
         ("C-g" . vterm--self-insert)))

(use-package vterm-toggle 
  :straight (vterm-toggle :type git
			  :host github
			  :repo "jixiuf/vterm-toggle")
  :bind (("<f2>" . vterm-toggle)
   	 ("C-<f2>" . vterm-toggle-cd)
	 :map vterm-mode-map(
			     "<f2>" . vterm-toggle))
  :config (setq vterm-toggle-fullscreen-p nil
;;		vterm-toggle-hide-method nil
		vterm-toggle-reset-window-configration-after-exit 'kill-window-only))
;;
;; (add-to-list 'display-buffer-alist
;;              '((lambda (buffer-or-name _)
;;                    (let ((buffer (get-buffer buffer-or-name)))
;;                      (with-current-buffer buffer
;;                        (or (equal major-mode 'vterm-mode)
;;                            (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
;;                 ;;(display-buffer-reuse-window display-buffer-at-bottom)
;;                 (display-buffer-reuse-window display-buffer-in-direction)
;;                 ;;display-buffer-in-direction/direction/dedicated iss added in emacs27
;;                 (direction . bottom)
;;                 (dedicated . t) ;dedicated is supported in emacs27
;;                 (reusable-frames . visible)
;;                 (window-height . 0.3)))

;;(remove-hook 'vterm-mode-hook 'evil-mode)

;; (use-package eshell-vterm
;;   :straight (eshell-vterm :type git
;; 			  :host github
;; 			  :repo "iostapyshyn/eshell-vterm")
;;   :after eshell
;;   :config
;;   (eshell-vterm-mode))

;;Runstuff
(use-package run-stuff
  :straight (run-stuff :files (:defaults "run-stuff")
		       :host nil
		       :type git
		       :repo "https://codeberg.org/ideasman42/emacs-run-stuff.git"))

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))


;; (use-package native-complete
;;   :straight (native-complete :type git
;; 			     :host github
;; 			     :repo "CeleritasCelery/emacs-native-shell-complete"))
;; (add-hook 'shell-mode-hook (lambda () (setq comint-prompt-regex "^.+[$%>] ")))
;; (with-eval-after-load 'shell
;;   (native-complete-setup-bash))
;;  :hook (eshell-mode . esh-autosuggest-mode))

;; (use-package esh-autosuggest
;;   :straight (esh-autosuggest :type git
;; 			     :host github
;; 			     :repo "dieggsy/esh-autosuggest")
;;   :hook (eshell-mode . esh-autosuggest-mode))



;;Prodigy

(use-package prodigy
  :straight (prodigy :type git
                     :host github
                     :repo "rejeep/prodigy.el")
  :defer 1
  :commands prodigy
  :bind (("<f12>" . prodigy)))


;;+------------+
;;|   Parser   |
;;+------------+
(use-package tree-sitter
  :hook (sh-mode . tree-sitter-hl-mode))


;((require 'tree-sitter)
(use-package tree-sitter-langs)

;;(global-tree-sitter-mode)
;; (use-package bicycle
;;   :straight (bicycle :type git
;; 		     :host github
;; 		     :repo "tarsius/bicycle")
;;   :after outline
;;   :bind (:map outline-minor-mode-map
;; 		([C-tab] . bicycle-cycle)
;; 		([S-tab] . bicycle-cycle-global)))

;;+----------------------------------+
;;|   Programming Language Support   |
;;+----------------------------------+
;;E-Lisp Develoment - The missing hash table library for Emacs.
(use-package prog-mode
  :straight(:type built-in)
  :hook((prog-mode . outline-minor-mode)
	(prog-mode . hs-minor-mode)))


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

;;Python
;; https://medium.com/analytics-vidhya/managing-a-python-development-environment-in-emacs-43897fd48c6a
(use-package elpy
    :straight t
    :bind
    (:map elpy-mode-map
          ("C-M-n" . elpy-nav-forward-block)
          ("C-M-p" . elpy-nav-backward-block))
    :hook ((elpy-mode . flycheck-mode)
           (elpy-mode . (lambda ()
                          (set (make-local-variable 'company-backends)
                               '((elpy-company-backend :with company-yasnippet))))))
    :init
    (elpy-enable)
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    ; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
    (setq elpy-shell-echo-output nil)
    (setq elpy-rpc-python-command "python3")
    (setq elpy-rpc-timeout 2))

(use-package buftra
  :straight (:host github :repo "humitos/buftra.el"))

(use-package py-pyment
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :config
    (setq py-pyment-options '("--output=numpydoc")))

(use-package py-isort
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-isort-enable-on-save)
    :config
    (setq py-isort-options '("--lines=88" "-m=3" "-tc" "-fgw=0" "-ca")))

(use-package py-autoflake
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-autoflake-enable-on-save)
    :config
    (setq py-autoflake-options '("--expand-star-imports")))

(use-package py-docformatter
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-docformatter-enable-on-save)
    :config
    (setq py-docformatter-options '("--wrap-summaries=88" "--pre-summary-newline")))

(use-package blacken
    :straight t
    :hook (python-mode . blacken-mode)
    :config
    (setq blacken-line-length '88))

(use-package python-docstring
    :straight t
    :hook (python-mode . python-docstring-mode))

;; (use-package pyenv
;;     :straight (:host github :repo "aiguofer/pyenv.el")
;;     :config
;;     (setq pyenv-use-alias 't)
;;     (setq pyenv-modestring-prefix " ")
;;     (setq pyenv-modestring-postfix nil)
;;     (setq pyenv-set-path nil)(global-pyenv-mode)
;;     (defun pyenv-update-on-buffer-switch (prev curr)
;;       (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
;;           (pyenv-use-corresponding)))
;;     (add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch))


;; (use-package python
;;   :straight (:type built-in)
;;   :hook (inferior-python-mode . fix-python-password-entry)
;;   :config
;;   (setq python-shell-interpreter "jupyter-console"
;;         python-shell-interpreter-args "--simple-prompt"
;;         python-shell-prompt-detect-failure-warning nil)
;;   (add-to-list 'python-shell-completion-native-disabled-interpreters
;;                "jupyter-console")
;;   (add-to-list 'python-shell-completion-native-disabled-interpreters
;;                "jupyter")

;;    (defun fix-python-password-entry ()
;;      (push
;;       'comint-watch-for-password-prompt comint-output-filter-functions))

;;    (defun my-setup-python (orig-fun &rest args)
;;      "Use corresponding kernel"
;;      (let* ((curr-python (car (split-string (pyenv/version-name) ":")))
;;             (python-shell-buffer-name (concat "Python-" curr-python))
;;             (python-shell-interpreter-args (if (bound-and-true-p djangonaut-mode)
;;                                                "shell_plus -- --simple-prompt"
;;                                              (concat "--simple-prompt --kernel=" curr-python)))
;;             (python-shell-interpreter (if (bound-and-true-p djangonaut-mode)
;;                                           "django-admin"
;; 					python-shell-interpreter)))
;;        (apply orig-fun args)))

;;    (advice-add 'python-shell-get-process-name :around #'my-setup-python)
;;    (advice-add 'python-shell-calculate-command :around #'my-setup-python))






;;C/C++

(use-package ccls
  :straight (ccls :type git :host github :repo "emacs-lsp/emacs-ccls")
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))





;;Common Lisp
(use-package slime
  :straight (slime :type git :host github :repo "slime/slime")
  :commands (slime)
  :init (setq inferior-lisp-program "sbcl"))




;;Rust
;;------
;; (use-package rust-mode
;;   :straight (rust-mode :type git :host github :repo "rust-lang/rust-mode"))

(use-package rustic
  :straight (rustic :type git :host github :repo "brotzeit/rustic")
  ;;  :ensure
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-kill-buffer-and-window t
	rustic-spinner-type 'progress-bar)
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

(remove-hook 'rustic-mode-hook 'flycheck-mode)

;; (with-eval-after-load 'rustic-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
;; (aviik/leadr-key-def
;;   :keymaps 'rustic-mode-map
;;   "c"  '(:ignore t :which-key "Rustic")
;;   ;; bind "C-' C-l"
;;   "ce" '(flycheck-list-errors :which-key "Flycheck List Errors")
;;   "cr" '(rustic-cargo-run :which-key "Cargo Run")
;;   "cb" '(rustic-cargo-build :which-key "Cargo Build")
;;   "ct" '(rustic-cargo-current-test :which-key "Run Test at Point")
;;   "C-t" '(rustic-cargo-test :which-key "Cargo Test")
;;   "cd" '(rustic-cargo-doc :which-key "Cargo Doc")
;;   "ca" '(rustic-cargo-add :which-key "Cargo Add")
;;   "cr" '(rustic-cargo-rm :which-key "Cargo Remove"))

;;Scheme - Guile
(use-package geiser
   :straight (geiser :type git :host gitlab :repo "emacs-geiser/geiser")
   )

(use-package geiser-racket
   :straight (geiser-racket :type git :host gitlab :repo "emacs-geiser/racket")
   )

(use-package macrostep-geiser
  :after (geiser-mode geiser-repl cider-mode)
  :hook (((geiser-mode
	   geiser-repl
	   cider-mode) . macrostep-geiser-setup)))


;;Haskell

(use-package lsp-haskell
    :straight (lsp-haskell :type git :host github :repo "emacs-lsp/lsp-haskell"))

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


;;+---------------------+
;;|   Syntax Checkers   |
;;+---------------------+

(use-package flymake
  :straight (flymake :type git :host github :repo "flymake/emacs-flymake")
  :config
  ;; I don't want no steekin' limits.
  (setq flymake-max-parallel-syntax-checks nil))

(use-package flycheck
  :straight (flycheck :type git :host github :repo "flycheck/flycheck")
  )



;;+---------------+
;;|   Debugging   |
;;+---------------+
(use-package dap-mode
  ;; :straight (:type built-in)
  :straight (dap-mode :type git
	    :host github
	    :repo "emacs-lsp/dap-mode")
  ;; :general (aviik/leadr-key-def
  ;; 	     "C-c C-d" '(dap-hydra :"Dap-Hydra"))
  :commands dap-debug
  :hook
    ((dap-mode . corfu-mode)
     (dap-terminated . lc/hide-debug-windows)
     (dap-session-created . (lambda (_arg) (projectile-save-project-buffers)))
     (dap-ui-repl-mode . (lambda () (setq-local truncate-lines t))))
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-tooltip-mode 1))
  ;; (use-package dap-cpptools
  ;;   :straight dap-mode)
  ;;   ;; (use-package dap-ui
  ;;   ;; :straight dap-mode)
  ;; (use-package dap-node
  ;;   :straight dap-mode
  ;;   :config (dap-node-setup))


(use-package dap-lldb
 :straight dap-mode)

;; https://github.com/WebFreak001/code-debug
(use-package dap-gdb-lldb
 :straight dap-mode
 :config (dap-gdb-lldb-setup))

(dap-register-debug-template "Rust::LLDB Run Configuration"
			     (list :type "lldb"
				   :request "launch"
				   :name "LLDB::Run"
				   :gdbpath "rust-lldb"
				   :target nil
				   :cwd nil))


;;+-------------+
;;|   Project   |
;;+-------------+
(use-package projectile
  :straight (projectile :type git :host github :repo "bbatsov/projectile")
  )
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(setq projectile-require-project-root nil)



(use-package project-x
  :straight (project-x :type git :host github :repo "karthink/project-x")
  :after project
  :config
  (setq project-x-save-interval 600)    ;Save project state every 10 min
  (project-x-mode 1))

;; (aviik/leadr-key-def
;;   "p"  '(:ignore t :which-key "Project")
;;   "pe" '(project-eshell)
;;   "ps" '(project-search)
;;   "pd" 'project-dired)




;;+-----------------------+
;;|   Emacs Development   |
;;+-----------------------+

;;The Bug Hunter
(use-package bug-hunter
  :straight (bug-hunter :type git :host github :repo "Malabarba/elisp-bug-hunter") 
  ;; :general(aviik/leadr-key-def
  ;; 	   "b" '(bug-hunter-init-file :which-key "Init File Debug"))
  )


(straight-use-package '(yodel :host github :repo "progfolio/yodel")
)
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

(use-package polymode
  :straight (polymode :type git :host github :repo "polymode/polymode")
  )



;;+--------------+
;;|   Treemacs   |
;;+--------------+



(use-package treemacs
  ;; :straight (treemacs :type git
  ;; 		      :host github
  ;; 		      :repo "Alexander-Miller/treemacs"
  ;; 		      :files (:defaults "src/*/*") ;;"src/elisp/*" "src/scripts/*")
  ;; 		      :includes (treemacs-evil
  ;; 				 treemacs-all-the-icons
  ;; 				 treemacs-magit))
  :defer t
  :bind (("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t d"   . treemacs-select-directory)
         ("C-x t B"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag))
  :config
  (progn
    (setq treemacs-workspace-switch-cleanup t)))
(use-package treemacs-evil
  :straight t
  :after (treemacs evil))

(use-package treemacs-icons-dired
  :straight t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))
  ;; :config
  ;;  (general-define-key
  ;;   :keymaps 'treemacs-mode-map
  ;;   [mouse-1] #'treemacs-single-click-expand-action
  ;;   "M-l" #'treemacs-root-down
  ;;   "M-h" #'treemacs-root-up
  ;;   "q" #'treemacs-quit)
  ;;  (general-define-key
  ;;   :keymaps 'treemacs-mode-map
  ;;   :states '(normal emacs)
  ;;   "q" 'treemacs-quit))

(use-package lsp-treemacs
  :straight (lsp-treemacs :type git :host github :repo "emacs-lsp/lsp-treemacs")
  :after (lsp)
  :commands (lsp-treemacs-errors-list)
  :config
  (lsp-treemacs-sync-mode 1))


;;+----------------------+
;;|   Org-Mode Support   |
;;+----------------------+
(straight-use-package '(org :type built-in))

(straight-use-package '(org-contrib :includes ob-ebnf
				    ob-arduino
				    org-annotate-file))



(with-eval-after-load 'org       
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (variable-pitch-mode 1)
  (setq evil-auto-indent nil)
  (add-hook 'org-mode-hook #'visual-line-mode))

;; Tweak font sizes
(setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                   (header-line (:height 4.0) variable-pitch)
                                   (org-document-title (:height 1.75) org-document-title)
                                   (org-code (:height 1.55) org-code)
                                   (org-verbatim (:height 1.55) org-verbatim)
                                   (org-block (:height 1.25) org-block)
                                   (org-block-begin-line (:height 0.7) org-block)))


(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight (visual-fill-column :type git
				:host nil
				:repo "https://codeberg.org/joostkremers/visual-fill-column")
  :defer t
  :hook (org-mode . my/org-mode-visual-fill))


(use-package org-bullets
  :straight (org-bullets :type git :host github :repo "sabof/org-bullets")
  :hook (org-mode . org-bullets-mode))


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

;; Notes / Tasks / TODOs

;; Make custom markers for todo items:

;; TODO
;;     something that needs to be done at some point. preferably add a date, but it may be moved. 
;; PENDING
;;     awaiting feedback from someone else or hasn’t been any feedback at that time. 
;; MEETING
;;     a scheduled meeting and cannot easily be rescheduled. 
;; DOING
;;     In progress, needs to be finished 
;; DONE
;;     done. 
;; CANCELED
;;     can be ignored. May include a note on why it’s been cancelled. 

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "PENDING(p)" "MEETING(m)" "|" "DONE(d)" "CANCELED(c)")))






;;+------------------------------+
;;|   Publishing with Org Mode   |
;;+------------------------------+
;;;Hugo
;;;Ox-hugo
;;;https://ox-hugo.scripter.co/
(use-package ox-hugo
  :straight(ox-hugo :type git
                    :host github
                    :repo "kaushalmodi/ox-hugo")
  :after ox)


;;;Presentations with Org-Reveal
;;;https://github.com/yjwen/org-reveal
(use-package ox-reveal
  :straight (ox-reveal :type git :host github :repo "yjwen/org-reveal"))






;;(when (bound-and-true-p aviik/for-org) (load-theme 'nano-light))

;;(require 'nano-help)

;;Mode line controlled by power line
(use-package doom-modeline
  :straight (doom-modeline :type git :host github :repo "seagle0128/doom-modeline")
  :hook (after-init . doom-modeline-mode)
  :config
   (setq doom-modeline-buffer-file-name-style 'buffer-name)
  ;; (set-face-attribute 'doom-modeline nil :family "Fira Code" :height 100
  ;; ;;                    'doom-modeline-inactive nil :family "Fira Code" :height 100)
  ;; 		      )
   (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange"))
;;(menu-bar-mode -1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "a137529f2b83537fda674b1723043f0e5fcac21f9d0b5ea4aa64443c281bac2b" "f0eb51d80f73b247eb03ab216f94e9f86177863fb7e48b44aacaddbfe3357cf1" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; ;--------------
;; ┏━┓╋┏┳━━━┳━┓┏┳━━━┳━━━┳━━┳━┓┏┓
;; ┃┃┃╋┃┃┏━┓┣┓┃┃┃┏━┓┃┏━┓┣┫┣┻┓┃┃┃
;; ┃┃┗┓┃┃┃╋┃┃┃┗┛┃┃╋┃┣┛╋┃┃┃┃╋┃┗┛┃
;; ┃┏┓┗┛┃┃╋┃┃┏┏┓┃┃╋┃┃╋╋┃┃┃┃╋┏┏┓┃
;; ┃┃┗┓┃┃┗━┛┣┛┃┃┃┗━┛┃╋╋┃┣┫┣┳┛┃┃┃
;; ┗┛╋┗━┻━━━┻━┛┗┻━━━┛╋╋┗┻━━┻━┛┗┛
;;+---------------------------+
;;|   `Early-Configuration'   |
;;+---------------------------+
;;(setq gc-cons-threshold most-positive-fixnum) ; avoid GC during startup to save time

(defalias 'yes-or-no-p 'y-or-n-p)


;;PERSONAL INFO
;;-------------

;;Personal information
(setq user-full-name "kilokon"
      user-mail-address "kilo.kon@outlook.com")

;;+---------------------------------------+
;;|  `Constants'`Environment-Variables'   |
;;+---------------------------------------+
(setenv "BROWSER" "firefox")
;;I keep pressing :wq
(defconst wq "Use C-x C-c")
(defvar kilo/backup-dir (expand-file-name "backups" user-emacs-directory))
(defvar kilo/oneDrive (expand-file-name "OneDrive" (getenv "HOME")))
(defconst kilo-key "<VoidSymbol>") 	;Using Capslock as VoidSymbol via Kmonad
(defvar kilo-win-env
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

;;+----------------+
;;|  `Interface'   |
;;+----------------+
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mode-line-format . 0) default-frame-alist)
(tool-bar-mode 0)                   ;; Disable the tool bar
(scroll-bar-mode 0)                 ;; Disable the scrollbar
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 7)
(global-hl-line-mode 1)

;;+-------------+
;;|  `Defuns'   |
;;+-------------+
;;----------
(defun aviik/reload-init-file()
  "It reloads Emacs init config."
  (interactive)
  (load-file user-init-file))

(defun aviik/switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

;; https://emacs.stackexchange.com/questions/63147/toggle-between-relative-and-absolute-line-numbers-lisp-syntax
(defun kilo/toggle-line-numbering ()
  "Toggle line numbering between absolute and relative."
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
    (setq display-line-numbers 'relative)))

;; Yank from history
(defun yank-hist-select ()
  (interactive)
  (insert (completing-read "Yank:" kill-ring)))

;;;http://chopmo.dk/2016/10/27/emacs-highlighting-current-word.html
;; (require 'hi-lock)
(defun aviik/toggle-mark-word-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))

;; https://www.emacswiki.org/emacs/CopyingWholeLines
(defun aviik/quick-copy-line ()
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

;; https://github.com/magnars/.emacs.d/blob/master/defuns/file-defuns.el
(defun aviik/copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new (if eproject-mode
                  (s-chop-prefix (eproject-root) filename)
                filename))))
;; hitting <C-S-backspace>, the binding for kill-whole-line appends to kill ring

(defun kilo/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Comment Uncomment Region
(defun kilo/comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    ))
;; https://www.emacswiki.org/emacs/lazycat-toolkit.el
(defun kilo/ielm-toggle ()
  "Toggle ielm buffer."
  (interactive)
  (require 'ielm)
  (let ((ielm-buffer-name "*ielm*"))
    (if (get-buffer ielm-buffer-name)
        (if (string-equal ielm-buffer-name (buffer-name))
            (bury-buffer)
          (switch-to-buffer ielm-buffer-name))
      (ielm))))

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

;; +-----------------+
;; |   `Super-Key'   |
;; +-----------------+

(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key
(current-word)



;; +----------------------+
;; |   'Elisp-Learning'   |
;; +----------------------+
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
;; This gets multiple choices
;; (completing-read "My Prompt: " '("red" "green" "blue") nil nil)

;; +----------------------+
;; |  'File-Management'   |
;; +----------------------+
;; This org babel line comes on top of file management
;; (org-babel-load-file (concat user-emacs-directory "conf.org"))

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
;; Backup File
(unless (file-exists-p kilo/backup-dir)
  (mkdir kilo/backup-dir))


(setq
 create-lockfiles nil        ;; Don't create lock files.
 backup-by-copying t      ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t       ; use versioned backups
 backup-directory-alist
 `((".*" . ,kilo/backup-dir))
 auto-save-file-name-transforms
 `((".*" ,kilo/backup-dir t)))




;; https://www.emacswiki.org/emacs/backup-each-save.el
;; (load "~/.emacs.d/scripts")
(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)



;; Snippets Directory
(defvar kilo-snippet-dir (expand-file-name "snippets" user-emacs-directory))



;; History
(savehist-mode)
;; Recent Files
(recentf-mode t)
(add-hook 'kill-emacs-hook #'recentf-cleanup)
;; +-------------------------------+
;; |   `Version-Greater-Than--28'   |
;; +-------------------------------+
(if (> emacs-major-version 28)
    (setq read-extended-command-predicate
	  #'command-completion-default-include-p))

;; +-----------------------+
;; |   `Sexp-Selection'    |
;; +-----------------------+
(delete-selection-mode 1)           ;; Replace region when inserting text
(global-subword-mode 1)             ;; Iterate through CamelCase words


;; +----------------+
;; |   `Defaults'   |
;; +----------------+
;; Default Settings
;; ----------------
(setq visible-bell 1                    ; Annoying Ding off
      ring-bell-function 'ignore
      inhibit-startup-screen t
      ;; initial-major-mode 'org-mode
      initial-major-mode 'org-mode
      initial-scratch-message ""
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox"
      debug-on-error t                  ; Debug
      edebug-all-forms t                ; Debug
      undo-limit 200000                 ; Undo Limits
      undo-strong-limit 40000000        ; Undo Limits
      load-prefer-newer t               ; Load Newer el files
      make-pointer-invisible t          ; Hide Mouse While editing
      package-enable-at-startup nil ; Disable package.el in favor of straight.el
      enable-recursive-minibuffers t    ; Enable recursive minibuffers
      tab-always-indent 'complete ; Enable indentation+completion using the TAB key.
      completion-cycle-threshold 3 ; TAB cycle if there are only few candidates
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;`End-Of-Defaults';;;;;;;;;;;;;;;;;;;;;




;;Auto-Save Management
;;--------------------
 
;;+-------------------------+
;;|  `Package-Management'   |
;;+-------------------------+
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;`End-Of-Package-Management';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; ;+-----------------------+
;; ;;|   `Time' and `Date'   |
;; ;;+-----------------------+
;; ;;https://github.com/alphapapa/ts.el
;; (use-package ts
;;   :straight (ts :type git :host github :repo "alphapapa/ts.el"))
;; emacs-evil/evil")
;;   )

(use-package evil
  :straight (evil :type git :host github :repo "emacs-evil/evil")
  :init (evil-mode 1)
  )







;;+-------------+
;;|   `Theme'   |
;;+-------------+
(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "doomemacs/themes")
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
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

(use-package modus-themes
  :straight (modus-themes :type git :host github :repo "protesilaos/modus-themes")
  )






;;+--------------+
;;|   `Cursor'   |
;;+--------------+
(blink-cursor-mode 0)
(setq-default cursor-type 'box)

(use-package multiple-cursors
  :straight (multiple-cursors :type git :host github :repo "magnars/multiple-cursors.el")
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" .  mc/mark-all-like-this)))

(use-package pulsar
  :straight t ;; (pulsar :type git :repo "https://git.sr.ht/~protesilaos/pulsar")
  ;; :hook ((consult-a))
  :config
  (setq pulsar-pulse-functions
	'( forward-page
           backward-page
           scroll-up-command
           scroll-down-command
	   org-next-visible-heading
           org-previous-visible-heading
           org-forward-heading-same-level
           org-backward-heading-same-level
           outline-backward-same-level
           outline-forward-same-level
           outline-next-visible-heading
           outline-previous-visible-heading
           outline-up-heading))
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1))


;;+--------------------------------+
;;|   `UI':`Frames' and `Window'   |
;;+--------------------------------+

(use-package burly
  :straight (burly :type git
		   :host github
		   :repo "alphapapa/burly.el"))



;;+-------------------------------+
;;|   `Ui': `Buffer-Management'   |
;;+-------------------------------+

;; (use-package iflipb
;;   :straight (iflipb :type git :host github :repo "jrosdahl/iflipb")
;;   :bind (("C-<VoidSymbol" . iflipb-next-buffer)))


;;+-----------------------------------+
;;|   `UI': 'Global-Font-Settings'    |
;;+-----------------------------------+
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :weight 'light :height 100)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :weight 'light :height 120)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.1)



;;+-------------+
;;|   `Modes'   |
;;+-------------+
(use-package diminish
  :straight (diminish :type git 
		      :host github 
		      :repo "emacsmirror/diminish")
  :config
  (diminish 'abbrev-mode)
  (diminish 'subword-mode)
  (diminish 'eldoc-mode " Ⓔ "))


;;+-----------------------+
;;|   `Version-Control'   |
;;+-----------------------+
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

;; Diff Hl
(use-package diff-hl
  :straight (diff-hl :type git :host github :repo "dgutov/diff-hl")
  :init (global-diff-hl-mode)
  :hook (diff-hl-mode . diff-hl-margin-mode))
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


;;+---------------------+
;;|   `Undo' & `Redo'   |
;;+---------------------+
(straight-use-package 'undo-fu)

;; Vundo
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


;;+--------------------+
;;|   `Key-Controls'   |
;;+--------------------+
;;Unset a few keys
;; (-map (lambda (x) (unbind-key x)) '("C-x C-d" ;; list-directory
;;                                     "C-z"     ;; suspend-frame
;;                                     "C-x C-z" ;; again
;;                                     "M-o"     ;; facemenu-mode
;; 				    "C-a"
;;                                     ;; "<mouse-2>" ;; pasting with mouse-wheel click
;;                                     ;; "<C-wheel-down>" ;; text scale adjust
;;                                     ;; "<C-wheel-up>" ;; ditto
;;                                     "s-n"     ;; make-frame
;;                                     "C-x C-q" ;; read-only-mode
;; 				    "M-m"
;; 				    "C-/"
;;                                     ))

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
;; :states '(insert emacs normal hybrid motion visual operator)
 :prefix "SPC"
 :non-normal-prefix "S-SPC"
 :keymaps 'override)


(general-create-definer kilo-leader-key
  :prefix kilo-key)


;; (use-package bind-map
;;   :straight t)

;; Settings come from
;;https://github.com/mohkale/emacs/blob/master/init.org#leader
(straight-use-package
 '(spaceleader :type git :host github :repo "mohkale/spaceleader"))


;;Global-Shortcuts
(global-set-key (kbd "C-c a r") 'aviik/reload-init-file)
(global-set-key (kbd "C-c a s") 'aviik/switch-to-scratch-buffer)
(global-set-key (kbd "C-c a r") 'recentf-open-files)
(global-set-key (kbd "C-c a t") 'load-theme)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)
;;(global-set-key (kbd "M-<up>") 'mode-line-other-buffer)
(global-set-key (kbd "C-<VoidSymbol>") 'other-window)
(global-set-key (kbd "s-.") 'aviik/toggle-mark-word-at-point)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "C-c c c") 'aviik/quick-copy-line)
(global-set-key (kbd "C-c C-w") 'kill-buffer-and-window)
(global-set-key (kbd "C-/") 'kilo/comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c y") 'yank-hist-select)
(global-set-key (kbd "<f6>") 'display-line-numbers-mode)
(global-set-key (kbd "C-<f6>") 'kilo/toggle-line-numbering)


;;+----------------+
;;|   `Bookmark'   |
;;+----------------+
(use-package linkmarks
  :straight (linkmarks :type git :host github :repo "dustinlacewell/linkmarks")
  :config )


;;+-----------+
;;|   `w3m'   |
;;+-----------+
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


;;+----------------------------------------------+
;;|  `Icons', `Emojis' and `Special-Characters'  |
;;+----------------------------------------------+
(use-package all-the-icons
  :straight (all-the-icons :type git
			   :host github
			   :repo "domtronn/all-the-icons.el"))


;;+-----------------------+
;;|       `Dired'         |
;;+-----------------------+
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

;; This is *NECESSARY* for Doom users who enabled `dired' module
;; (map! :map dired-mode-map :ng "q" #'quit-window)

(use-package dirvish
    :straight (dirvish :type git 
                :host github 
                :repo "alexluigit/dirvish")
  :init
  (dirvish-override-dired-mode)
  :custom
  ;; Go back home? Just press `bh'
  (dirvish-bookmark-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")))
  ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format	      ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; Don't worry, Dirvish is still performant even you enable all these attributes
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  ;; Maybe the icons are too big to your eyes
  ;; (dirvish-all-the-icons-height 0.8)
  ;; In case you want the details at startup like `dired'
  ;; (dirvish-hide-details nil)
  :config
  (dirvish-peek-mode)
  ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  ;; Enable mouse drag-and-drop files to other applications
  (setq dired-mouse-drag-files t)	; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  ;; Make sure to use the long name of flags when exists
  ;; eg. use "--almost-all" instead of "-A"
  ;; Otherwise some commands won't work properly
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dired-mode-map ; Dirvish respects all the keybindings in this map
   ;; ("h" . dired-up-directory)
   ;; ("j" . dired-next-line)
   ;; ("k" . dired-previous-line)
   ;; ("l" . dired-find-file)
   ;; ("i" . wdired-change-to-wdired-mode)
   ;; ("." . dired-omit-mode)
   ("b"   . dirvish-bookmark-jump)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("s"   . dirvish-quicksort)	; remapped `dired-sort-toggle-or-edit'
   ("?"   . dirvish-dispatch)	; remapped `dired-summary'
   ("TAB" . dirvish-subtree-toggle)
   ("SPC" . dirvish-history-jump)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))


(use-package dired-x
  :straight (:type built-in)
  ;; Enable dired-omit-mode by default
  ;; :hook
  ;; (dired-mode . dired-omit-mode)
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Some tips to speed up Dired/Dirvish over TRAMP
(use-package tramp
  :straight (:type built-in)
  :config
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  (setq tramp-verbose 0)
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp/"))
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil))

;; Addtional syntax highlighting for dired
(use-package diredfl
    :straight (diredfl :type git 
                :host github 
                :repo "purcell/diredfl")

  :hook
  (dired-mode . diredfl-mode))




(use-package zoxide
  :straight (zoxide :type git
		    :host gitlab
		    :repo "Vonfry/zoxide.el")
  :general (aviik/leadr-key-def
	     "gz" 'zoxide-find-file))

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

;;+---------------------+
;;|   `Env-Variables'   |
;;+---------------------+
(use-package direnv
    :straight (direnv :type git 
                :host github 
                :repo "wbolster/emacs-direnv")
  :config
  (direnv-mode))

;;+-------------------+
;;|   `File-Rights'   |
;;+-------------------+
;; (use-package sudo-edit
;;   :straight (sudo-edit :type git :host github :repo "nflath/sudo-edit"))





(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  :bind
  (("C-." . embark-act)	;; pick some comfortable binding
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

(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)




;;+-----------------------+
;;| `Completion-System'   |
;;+-----------------------+
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
  (vertico-count 13)		     ; Number of candidates to display
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


;;+------------------------+
;;|   `Narrowing-System'   |
;;+------------------------+

;; fzf provider for consult
(use-package affe
    :straight (affe :type git 
                :host github 
                :repo "minad/affe")
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package consult
  :straight (consult :type git :host github :repo "minad/consult")
  :bind (("C-s" . consult-line)
	 ("M-y" . consult-yank-pop))
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

;; consult extension for project.el
(use-package consult-project-extra
  :straight (consult-project-extra :type git
				   :host github
				   :repo "Qkessler/consult-project-extra")
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

;; Some connections to `Pulsar'
;; integration with the `consult' package:
(add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
(add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)

;; integration with the built-in `imenu':
(add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
(add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)

;;+---------------------------------------------+
;;|   `Completion-Content-Filtering/Ordering'   |
;;+---------------------------------------------+

(use-package orderless
  :straight (orderless :type git :host github :repo "oantolin/orderless")
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides nil
        completion-ignore-case t))
  
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



;;+---------------------------+
;;|   `Completion-System'     |
;;+---------------------------+
;;https://codeberg.org/takeonrules/dotemacs/src/branch/main/emacs.d/configuration.org
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  ;; :hook (after-init-hook . global-corfu-mode)
  ;; :defer t
  :custom (
	   (corfu-auto t)  ;; Enable auto completion
	   (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
	   (corfu-min-width 80)
	   (corfu-max-width corfu-min-width))
  :bind
  (:map corfu-map
	("<escape>". corfu-quit)
	("<return>" . corfu-insert)
	("M-d" . corfu-show-documentation)
	("M-l" . 'corfu-show-location)
	("TAB" . corfu-next)
	;; ([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  
  ;;    Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  ;; :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  ;; (tab-always-indent 'complete)
  ;; (completion-cycle-threshold nil)    ; Always show candidates in menu
  ;; (corfu-auto t) 
  ;; (setq-local corfu-auto t
  ;;             corfu-auto-delay 0
  ;;             corfu-auto-prefix 0
  ;;             completion-styles '(orderless-fast))
  ;; (corfu-auto t "Enable auto completion")
  ;; ;; ;; (corfu-auto-delay 0)
  ;; ;; (corfu-auto-prefix 0)
  ;; (completion-styles '(orderless-fast))
  ;; (corfu-auto-prefix 2)
  ;; (corfu-auto-delay 0.25)
  
  ;; (corfu-min-width 80)
  ;; (corfu-max-width corfu-min-width)     ; Always have the same width
  ;; (corfu-count 14)
  
  ;; (corfu-scroll-margin 4)
  ;; (corfu-cycle nil)
  
  ;; (corfu-echo-documentation nil)        ; Already use corfu-doc
  ;; (corfu-separator ?\s)
					; Necessary for use with orderless
  ;; (corfu-quit-no-match 'separator)
  ;; ;; (corfu-preview-current 'insert)       ; Preview current candidate?
  ;; ;; (corfu-preselect-first t)
  ;; 					; Preselect first candidate?
  
  :init
  (global-corfu-mode)
  )

(use-package dabbrev
  :straight (:type built-in)
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; (use-package fancy-dabbrev
;;   :straight (fancy-dabbrev :type git :host github :repo "jrosdahl/fancy-dabbrev")
;;   :custom
;;   (global-fancy-dabbrev-mode))

;; (global-set-key (kbd "TAB") 'fancy-dabbrev-expand-or-indent)
;; (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)


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








(use-package cape
  :straight (cape :type git :host github :repo "minad/cape")
  :after (corfu)
  :demand t
  :commands (cape-history ; Complete from Eshell, Comint or minibuffer history
             cape-file
             cape-keyword      ; Complete programming language keyword
             cape-tex ; Complete unicode char from TeX command, e.g. \hbar.
             cape-abbrev      ; Complete abbreviation at point
             ;; cape-dict	      ; Complete word from dictionary at point
             cape-line ; Complete current line from other lines in buffer
             cape-symbol	  ; Elisp symbol
             cape-ispell	  ; Complete word at point with Ispell
             ;; Complete with Dabbrev at point
             cape-dabbrev)
  :custom
  (cape-dabbrev-min-length 3)
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-super-capf #'cape-dabbrev #'cape-ispell #'cape-symbol #'cape-file #'cape-history ))))))




  (straight-use-package
   '(popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))


(use-package corfu-terminal
  :straight (corfu-terminal :type git
			    :host nil
			    :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(unless (display-graphic-p)
  (corfu-terminal-mode +1))


;;+------------------------+
;;|  `Snippet-Management'  |
;;+------------------------+
;;Yasnippet
(use-package yasnippet
  :straight (yasnippet :type git
		       :host github
		       :repo "joaotavora/yasnippet")
  ;;:after smart-tab
  ;; :general
  ;; ("C-;" 'yas-insert-snippet)
  :custom
    (setq yas-snippet-dirs '(expand-file-name kilo-snippet-dir))
  ;; (use-package yasnippet-snippets
  ;;   :straight (yasnippet-snippets :type git
  ;; 				  :host github
  ;;			  :repo "AndreaCrotti/yasnippet-snippets"))
  :config  
    (yas-global-mode 1))


(use-package consult-yasnippet
  :straight (consult-yasnippet :type git
			       :host github
			       :repo "mohkale/consult-yasnippet")
;;  :after (consult yasnippet)
  :bind ("C-;" . consult-yasnippet))


;; Configure Tempel
(use-package tempel
  :straight (tempel :type git
		    :host github
		    :repo "minad/tempel")
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

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )


;;+-----------------------+
;;|   `Language-Server'   |
;;+-----------------------+
(use-package lsp-mode
  :straight (lsp-mode :type git
                      :host github
                      :repo "emacs-lsp/lsp-mode")
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;;   (lsp-eldoc-render-all t)
  ;;   (lsp-idle-delay 0.6)
  ;;     ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-diagnostics-provider :capf)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  (lsp-lens-enable nil)
  (lsp-disabled-clients '((python-mode . pyls)))
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  :general (kilo-leader-key
	     ;; :keymaps 'lsp-mode-map
	     "ld" '(xref-find-definitions   :which-key "Find Definitions")
	     "lr" '(xref-find-references    :which-key "Find References")
	     "lq" '(lsp-workspace-restart   :which-key "Restart Lsp")
	     "lx" '(lsp-workspace-shutdown  :which-key "Kill Lsp Server")
	     "la" '(lsp-execute-code-action :which-key "Execute Code Action")))

(use-package lsp-ui
  :straight (lsp-ui :type git
                    :host github
                    :repo "emacs-lsp/lsp-ui")
  :commands lsp-ui-mode
  :bind
  (:map lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-j" . lsp-ui-peek-find-definitions)
        ;; ("C-c m"   . lsp-ui-imenu)
        ("C-c s"   . lsp-ui-sideline-mode)
        ("C-c d"   . aviik/toggle-lsp-ui-doc))
  :custom
;;  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-code-actions-prefix ""))

(setq lsp-completion-provider :none)
(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-mode-hook #'corfu-lsp-setup)

(use-package consult-lsp
  :straight (consult-lsp :type git
			 :host github
			 :repo "gagbo/consult-lsp")
  :general (kilo-leader-key
	    :keymaps 'lsp-mode-map
	    "c"   '(:ignore t                 :which-key "Consult-Lsp")
	    "cd"  '(consult-lsp-diagnostics   :which-key "Lsp Diagnostics")
	    "ci"  '(consult-imenu             :which-key "Consult Imenu")
	    "cf"  '(consult-flymake           :which-key "Consult Flymake")
	    "cs"  '(consult-lsp-symbols       :which-key "Consult Lsp Symbols")
	    ;; "cfa" '(consult-lsp-file-symbols  :which-key "Consult Lsp File Symbols")
	    ))

;;Eglot
(use-package eglot
  :straight (eglot :type git :host github :repo "joaotavora/eglot")
  :commands eglot
  :custom
  ;; (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  ;;  (eglot-stay-out-of '(flymake))
  (eglot-autoshutdown t)
  ;; :hook
  ;; (eglot-managed-mode . eldoc-box-hover-mode)
  )
;;  (eglot-managed-mode . fk/company-enable-snippets)
;;  (eglot-managed-mode . (lambda () (flymake-mode 0)))
  ;; :config
  ;; (with-eval-after-load 'eglot
  ;;   (load-library "project")))

(use-package eglot-x
  :straight (eglot-x :type git :host github :repo "nemethf/eglot-x")
  :after (eglot)
  :bind (("s-." . eglot-x-find-refs)
	 ("C-c l r" . eglot-ask-runnables) 
	 ("C-c <up>" . eglot-x-move-item-up)
	 ("C-c <down>" . eglot-x-move-item-down)
	 ("C-c l d" . eglot-x-open-external-documentation)))


(use-package consult-eglot
  :straight (consult-eglot :type git :host github :repo "mohkale/consult-eglot")
  :after (eglot))

;; (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)

;;Eldoc Box
;; (use-package eldoc-box
;;   ;; :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
;;   :hook (eglot-managed-mode . eldoc-box-hover-mode)
;;   :custom
;;   (eldoc-box-clear-with-C-g t))


;; (add-to-list 'eglot-server-programs
;; 	     `
;;+-----------------+
;;|   `Debugging'   |
;;+-----------------+
(use-package dap-mode
  ;; :straight t
  :straight (dap-mode :type git
		      :host github
		      :repo "emacs-lsp/dap-mode"
		      :files ("*.el"))
  ;; :general (aviik/leadr-key-def
  ;; 	     "C-c C-d" '(dap-hydra :"Dap-Hydra"))
  :commands dap-debug
  :hook
  ((dap-mode . corfu-mode)
   (dap-ui-repl-mode . (lambda () (setq-local truncate-lines t))))
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-ui)
  ;; :straight dap-mode
  ;; :after (dap-mode))
  (require 'dap-cpptools)
  ;; :straight dap-mode
  ;; :after (dap-mode))
  (require 'dap-hydra)
  ;; (require 'dap-python)
  (require 'dap-lldb)
  ;; :straight dap-mode
  ;; :after (dap-mode))
  (require 'dap-gdb-lldb)
  (dap-register-debug-template "Rust::LLDB Run Configuration"
			       (list :type "lldb"
				     :request "launch"
				     :name "LLDB::Run"
				     :gdbpath "rust-lldb"
				     :target nil
				     :cwd nil)))

;;+--------------------------+
;;|   `Parenthesis-System'   |
;;+--------------------------+
;; auto-close parentheses
;; (electric-pair-mode +1)

(straight-use-package 'smartparens)
(require 'smartparens-config)

;; Always start smartparens mode in rust-mode, python-mode.
(add-hook 'rustic-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
;;+-----------------------------------------+
;;|   Garbage Collection & Parser Priming   |
;;+-----------------------------------------+

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil)

;;+-----------------------------------+
;;|   Terminals & External Payloads   |
;;+-----------------------------------+

(use-package eshell
  :straight (:type built-in)
;;  :commands (aviik/toggle-eshell)
  ;; :custom (setq eshell-aliases-file (expand-file-name "/eshell/aliases" user-emacs-directory)
  ;; 		eshell-directory-name "~/.dotfiles")
  ;; :general
  ;; (aviik/leadr-key-def
  ;;  "e" '(:ignore t :which-key "Eshell")
  ;;  "ee" #'aviik/toggle-eshell)
  :config
  (setq eshell-cmpl-ignore-case t
	eshell-cd-on-directory t
	eshell-prompt-function (lambda ()
				 (concat
				  (propertize " λ " 'face '(:foreground "#f75f5f"))
				  (propertize " ॐ " 'face `(:foreground "white"))
				  (propertize " ❯ " 'face '(:foreground "#f75f5f"))))
	;; eshell-prompt-function (lambda nil
	;; 			 (concat
	;; 			  (propertize (format "[%s]" (abbreviate-file-name (eshell/pwd))) 'face '(:foreground "#8787af"))
	;; 			  ;; (propertize " λ " 'face '(:foreground "#f75f5f"))
	;; 			  (propertize " ॐ " 'face `(:foreground "white"))
	;; 			  (propertize " ❯ " 'face '(:foreground "#f75f5f"))))
	;; eshell-prompt-regexp "^λ "
	eshell-history-size         50
	eshell-buffer-maximum-lines 300
	eshell-hist-ignoredups t
	eshell-highlight-prompt t
	eshell-save-history-on-exit t
        eshell-destroy-buffer-when-process-dies t
	eshell-scroll-to-bottom-on-input t
	eshell-prefer-lisp-functions nil))

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(add-hook 'eshell-mode-hook (lambda ()
			      (eshell/alias "ff" "find-file $1")
			      (eshell/alias "ee" "find-file-other-window $1")
			      (eshell/alias "gd" "magit-diff-unstaged")
			      (eshell/alias "gds" "magit-diff-staged")
			      (eshell/alias "d" "dired $1")
			      ;;			      (eshell/alias "cdO" (concat cd " ~/OneDrive"))
			      (eshell/alias "ll" "ls -la")))



(use-package eshell-info-banner
  :straight (eshell-info-banner :type git
				:host github
				:repo "Phundrak/eshell-info-banner.el")
  :ensure t
  :defer t
  :hook (eshell-banner-load . eshell-info-banner-update-banner))

(use-package eshell-syntax-highlighting
  :straight (eshell-syntax-highlighting :type git 
					:host github 
					:repo "akreisher/eshell-syntax-highlighting")
  :after eshell-mode
  :ensure t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))


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



;;Path to Shell
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :type git
				  :host github
				  :repo "purcell/exec-path-from-shell")
  :init (exec-path-from-shell-initialize))



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

;;Runstuff
(use-package run-stuff
  :straight (run-stuff :files (:defaults "run-stuff")
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


;;+------------+
;;|   Parser   |
;;+------------+
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(require 'tree-sitter)
(require 'tree-sitter-langs)


;;+---------------------+
;;|   `View-Asm-Code'   |
;;+---------------------+
(use-package rmsbolt
  :straight (rmsbolt :type git :host gitlab :repo "jgkamat/rmsbolt"))




;;+----------------------------------+
;;|   Programming Language Support   |
;;+----------------------------------+

(use-package prog-mode
  :straight(:type built-in)
  :hook((prog-mode . outline-minor-mode)
	(prog-mode . hs-minor-mode)))




;;;;;;E-Lisp;;;;;;;;

;;Hash Table for elisp
;; (use-package ht
;;   :straight (ht :type git :host github :repo "Wilfred/ht.el"))

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



;; (use-package lispyville
;;   :straight (lispyville :type git :host github :repo "noctuid/lispyville")
;;   :hook (lispy-mode . lispyville-mode))



;;;;;;;;Python;;;;;;;;;;
(use-package python-mode
  :straight (:type built-in)
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))




;; (use-package lsp-jedi
;;   :straight (lsp-jedi :type git 
;;                       :host github 
;;                       :repo "fredcamps/lsp-jedi")

;;   :after lsp)

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp-deferred))))

;; (add-hook 'python-mode-hook 'dap-python)
;; https://medium.com/analytics-vidhya/managing-a-python-development-environment-in-emacs-43897fd48c6a
;; (use-package elpy
;;   :straight (elpy :type git
;; 		  :host github
;; 		  :repo "jorgenschaefer/elpy")
;;   :bind
;;   (:map elpy-mode-map
;;         ("C-M-n" . elpy-nav-forward-block)
;;         ("C-M-p" . elpy-nav-backward-block))
;;   ;; :hook ((elpy-mode . flycheck-mode)
;;   ;;        (elpy-mode . (lambda ()
;;   ;;                       (set (make-local-variable 'company-backends)
;;   ;;                            '((elpy-company-backend :with company-yasnippet))))))
;;   :init
;;   (elpy-enable)
;;   :config
;;   ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;; 					; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
;;   (setq elpy-shell-echo-output nil)
;;   (setq elpy-rpc-python-command "python3")
;;   (setq elpy-rpc-timeout 2)
;;   (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt"))

;; (when (load "flycheck" t t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))


(use-package python-pytest
  :straight (python-pytest :type git
			   :host github
			   :repo "wbolster/emacs-python-pytest")
  :after (python-mode))


(add-hook 'python-mode-hook
          (lambda ()
            (when-let ((r (locate-dominating-file default-directory ".pyroot")))
              (setq python-pytest-executable
                    (concat "PYTHONPATH=" r " " "pytest")))))

;; (use-package pyinspect
;;   :straight (pyinspect :type git
;; 		       :host github
;; 		       :repo "it-is-wednesday/pyinspect.el")
;;   :after (python-mode)
;;   :bind (:map python-mode-map
;; 	      ("C-c i" . pyinspect-inspect-at-point)))



;; (use-package buftra
;;   :straight (:host github :repo "humitos/buftra.el"))

;; (use-package py-pyment
;;     :straight (:host github :repo "humitos/py-cmd-buffer.el")
;;     :config
;;     (setq py-pyment-options '("--output=numpydoc")))

(use-package python-isort
  :straight (python-isort :host github :repo "wyuenho/emacs-python-isort")
  :hook (python-mode . python-isort-on-save-mode)
  ;; :config
  ;; (setq py-isort-options '("--lines=88" "-m=3" "-tc" "-fgw=0" "-ca"))
  )

;; (use-package py-autoflake
;;     :straight (:host github :repo "humitos/py-cmd-buffer.el")
;;     :hook (python-mode . py-autoflake-enable-on-save)
;;     :config
;;     (setq py-autoflake-options '("--expand-star-imports")))

;; (use-package py-docformatter
;;     :straight (:host github :repo "humitos/py-cmd-buffer.el")
;;     :hook (python-mode . py-docformatter-enable-on-save)
;;     :config
;;     (setq py-docformatter-options '("--wrap-summaries=88" "--pre-summary-newline")))

(use-package blacken
  :straight t
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '79))

;; (use-package python-docstring
;;     :straight t
;;     :hook (python-mode . python-docstring-mode))

;;https://blog.fredrikmeyer.net/2020/08/26/emacs-python-venv.html
;;https://ddavis.io/posts/emacs-python-lsp/
;;https://github.com/aiguofer/pyenv.el
(use-package pyenv
  :straight (:type git :host github :repo "aiguofer/pyenv.el")
  :init
  (setq pyenv-installation-dir "/usr")
  :config
  (global-pyenv-mode)
  (setq pyenv-modestring-prefix " ")
  (setq pyenv-use-alias 't)
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python")))))
;;   (setq pyenv-installation-dir "pyenv")
;;   (setq pyenv-use-alias 't)
;;   (setq pyenv-modestring-prefix " ")
;;   (setq pyenv-modestring-postfix nil)
;;   (setq pyenv-set-path nil)(global-pyenv-mode)
;;   (defun pyenv-update-on-buffer-switch (prev curr)
;;     (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
;;         (pyenv-use-corresponding)))
;;   (add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch))


(use-package python
  :straight (:type built-in)
;  :hook (inferior-python-mode . fix-python-password-entry)
  :config
  (setq python-shell-interpreter "/opt/hfs19.5.303/bin/hython"
        ;; python-shell-interpreter-args "--simple-prompt"
        python-shell-prompt-detect-failure-warning nil))






;; For C/C++ development.
;; (add-to-list 'eglot-server-programs '((c-mode c++-mode). ("ccls")))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)


;; (use-package ccls
;;   :straight (ccls :type git :host github :repo "emacs-lsp/emacs-ccls")
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))





;;Common Lisp
;; (use-package slime
;;   :straight (slime :type git
;; 		   :host github
;; 		   :repo "slime/slime")
;;   :commands (slime)
;;   :init (setq inferior-lisp-program "sbcl"))

(use-package sly
  :straight (sly :type git :host github :repo "joaotavora/sly")
  :init (setq inferior-lisp-program "sbcl")
  :bind (:map sly-mode-map
	      ("s-h" . sly-documentation-lookup)))

;; (use-package sly-autoloads
;;   :straight (sly-autoloads :type git :host github :repo "joaotavora/sly"))


(use-package lispy
  :straight (lispy :type git :host github :repo "abo-abo/lispy")
  :hook ((emacs-lisp-mode
	  sly-mrepl-mode
	  lisp-mode
	  geiser-repl) . lispy-mode)
  :bind (("C-)" . lispy-slurp)
	 ("C-(" . lispy-barf)))

;;`Rust'
;;------
;; (use-package rust-mode
;;   :straight (rust-mode :type git :host github :repo "rust-lang/rust-mode"))

(use-package rustic
  :straight (rustic :type git
		    :host github
		    :repo "brotzeit/rustic")
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
	     "ra"  '(rustic-cargo-add          :which-key "Cargo Add")
	     "rm"  '(rustic-cargo-rm           :which-key "Cargo Remove")))

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
   :straight (geiser :type git
		     :host gitlab
		     :repo "emacs-geiser/geiser"))

(use-package geiser-guile
   :straight (geiser-guile :type git
			   :host gitlab
			   :repo "emacs-geiser/guile"))

(use-package macrostep-geiser
  :after (geiser-mode geiser-repl cider-mode)
  :hook (((geiser-mode
	   geiser-repl
	   cider-mode) . macrostep-geiser-setup)))


;;Haskell

;; (use-package lsp-haskell
;;     :straight (lsp-haskell :type git
;; 			   :host github
;; 			   :repo "emacs-lsp/lsp-haskell"))

(use-package haskell-mode
  :straight (haskell-mode :type git
			  :host github
			  :repo "haskell/haskell-mode")
  ;; :init((custom-set-variables
  ;;         '(haskell-process-suggest-remove-import-lines t)
  ;;         '(haskell-process-auto-import-loaded-modules t)
  ;;         '(haskell-process-log t)))
  :bind (:map haskell-mode-map
	      ("C-c h" . hoogle))
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


;;+------------------+
;;|   Data Formats   |
;;+------------------+
;; Good toml Parser
(use-package toml
  :straight (toml :type git
		  :host github
		  :repo "gongo/emacs-toml"
		  :branch "support-toml_0_2_0"))



;;Major mode for editing JSON files of any size
(use-package jsonian
  :straight (jsonian :type git
		     :host github
		     :repo "iwahbe/jsonian")
  :after so-long  
  :custom
  (jsonian-no-so-long-mode))


;;+-----------------------+
;;|   `Syntax-Checkers'   |
;;+-----------------------+





;;+---------------+
;;|   `Project'   |
;;+---------------+
(use-package projectile
  :straight (projectile :type git
			:host github :repo "bbatsov/projectile")
  )
(projectile-mode +1)
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
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









;;+------------------------+
;;|   `Org-Mode-Support'   |
;;+------------------------+
;; (straight-use-package '(org :type built-in))


;; Use newest org
;; (straight-use-package 'org)
(use-package org
  :straight t
  ;; :init  (setq org-directory (expand-file-name "org" kilo/oneDrive)
  ;; org-default-notes-file (concat org-directory "/notes.org"))
  :bind ("C-c c" . org-capture)
  :hook (
	 ((org-mode . flyspell-mode)
	  (org-mode . visual-line-mode)
	  (org-mode . variable-pitch-mode)))
  ;; :hook ((org-mode . variable-pitch-mode)
  ;; 	 (org-mode . visual-line-mode)
  ;; 	 ;; (org-mode . flyspell)
  ;; 	 )
  :custom ;; Tweak font sizes
  (org-directory (expand-file-name "org" kilo/oneDrive))
  (org-default-notes-file (concat org-directory "/notes.org"))
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  
  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 40)
     (internal-border-width . 40)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))  
  :config (setq org-startup-indented t ;; Enable `org-indent-mode' by default
		;; https://github.com/jakebox/jake-emacs/blob/main/jake-emacs/init.org
		org-latex-listings t
		org-latex-compiler "xelatex"
		org-export-with-broken-links t
		org-export-with-smart-quotes t
		org-export-backends '(ascii beamer html latex md odt)
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
		"⭠ now ─────────────────────────────────────────────────"))
;;; https://github.com/minad/org-modern

(use-package flyspell-correct
      :straight (flyspell-correct :type git 
                  :host github 
                  :repo "d12frosted/flyspell-correct")

    :after flyspell
    :bind (:map flyspell-mode-map ("C-:" . flyspell-correct-wrapper)))


(use-package flyspell-correct-popup
  :straight (flyspell-correct-popup :type git 
				    :host github 
				    :repo "d12frosted/flyspell-correct")
  :after flyspell-correct)


(straight-use-package '(org-contrib :includes ob-ebnf
				    ob-arduino
				    org-annotate-file))

(defun kilo/create-notes-file ()
 "Create an org file in ~/notes/."
 (interactive)
 (let ((name (read-string "Filename: ")))
   (expand-file-name (format "%s.org"
			       name) "/home/aviik/Downloads/temp/")))





(setq org-capture-templates
      '(("n"				;;`org-capture' binding + n
	 "Notes"                        ;; Description   
	 entry                          ;; 
	 (file kilo/create-notes-file)
	 "#+TITLE%?\n  %U")

	;; ("d" "Demo Template" entry)
	("t" "Todo" entry (file+headline "gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))


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
      '((sequence "TODO(t)" "WIP(w)" "PENDING(p)" "REVIEW(r)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("WIP" . "yellow")
        ("PENDING" . "purple")
        ("REVIEW" . "orange")
        ("DONE" . "green")
        ("CANCELED" .  "red")))
;; (with-eval-after-load 'org       
;;   (setq org-startup-indented t)	 ; Enable `org-indent-mode' by default
;;   (variable-pitch-mode 1)
;;   ;; (setq evil-auto-indent nil)
;;   (add-hook 'org-mode-hook #'visual-line-mode))






(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (haskell . t)
   (calc . t)
   (ledger . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)))



(use-package visual-fill-column
  :straight (visual-fill-column :type git
				:host nil
				:repo "https://codeberg.org/joostkremers/visual-fill-column")
  :defer t
  :hook (org-mode . kilo/org-mode-visual-fill))


(use-package org-bullets
  :straight (org-bullets :type git :host github :repo "sabof/org-bullets")
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; (use-package evil-org
;;   :straight (evil-org :type git :host github :repo "Somelauw/evil-org-mode")
;;   :after org
;;   :hook (org-mode . (lambda () evil-org-mode))
;;   :config  (require 'evil-org-agenda)
;;            (evil-org-agenda-set-keys))

;;https://github.com/tecosaur/org-pandoc-import
(use-package org-pandoc-import
  :straight (:host github
             :repo "tecosaur/org-pandoc-import"
             :files ("*.el" "filters" "preprocessors")))






(eval-after-load "ox-latex"

  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure
  '(add-to-list 'org-latex-classes
                `("beamer"
                  ,(concat "\\documentclass[presentation]{beamer}\n"
                           "[DEFAULT-PACKAGES]"
                           "[PACKAGES]"
                           "[EXTRA]\n")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


;;+--------------------------------------+
;;|   `Notes-Management-with-Org-Mode'   |
;;+--------------------------------------+
;; https://blog.jethro.dev/posts/how_to_take_smart_notes_org/

(use-package org-roam
  :straight (org-roam :type git
		      :host github
		      :repo "org-roam/org-roam"		    
		      :files ("extensions/*" "*.el" "out"))
  :custom
  (org-roam-directory (expand-file-name "notes" org-directory))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("m" "main" plain
      "%?"
      :if-new (file+head "main/${slug}.org"
                         "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :if-new
      (file+head "reference/${title}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("a" "article" plain "%?"
      :if-new
      (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
      :immediate-finish t
      :unnarrowed t)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))




(use-package org-roam-ui
  :straight  (org-roam-ui :host github
			  :repo "org-roam/org-roam-ui"
			  :branch "main"
			  :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(use-package org-roam-bibtex
    :straight (org-roam-bibtex :type git 
			       :host github 
			       :repo "org-roam/org-roam-bibtex"
			       :files ("*.el" "out"))
  :after org-roam
  :config
  (require 'org-ref)) 


;;+------------------------------+
;;|   `Org-Mode'-``Publishing'   |
;;+------------------------------+
;;;Hugo
;;;Ox-hugo
;;;https://ox-hugo.scripter.co/
(use-package ox-hugo
  :straight(ox-hugo :type git
                    :host github
                    :repo "kaushalmodi/ox-hugo")
  :after ox)


;; Populates only the EXPORT_FILE_NAME property in the inserted heading.
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))
)


;;+---------------------------------+
;;|   `Org-Mode': `Presentations'   |
;;+---------------------------------+
(use-package org-tree-slide
  :straight (org-tree-slide :type git :host github :repo "takaxp/org-tree-slide")
  )




;;;Presentations with Org-Reveal
;;;https://github.com/yjwen/org-reveal
(use-package ox-reveal
  :straight (ox-reveal :type git :host github :repo "yjwen/org-reveal"))



;; (straight-use-package '(auctex :source el-get
;;                         :files ("*.el" "*.info" "dir"
;;                                 "doc" "etc" "images" "latex" "style")))
;; See the :load bits of
;; https://github.com/dimitri/el-get/blob/master/recipes/auctex.rcp,
;; which are not supported by straight.el as of this writing.  Without
;; these you will get built-in Emacs LaTeX modes, not AUCTeX.
;; (require 'tex-site)
;; (require 'preview-latex)


;;(when (bound-and-true-p aviik/for-org) (load-theme 'nano-light))

;;(require 'nano-help)

;;Mode line controlled by power line

;;+--------------------------------------+
;;|   `Org-Mode':`Outside-of-Org-Mode'   |
;;+--------------------------------------+

(use-package outshine
  :straight (outshine :type git
		      :host github
		      :repo "alphapapa/outshine")
  :hook (emacs-lisp-mode . outshine-mode))


;;+------------------+
;;|   UI: Modeline   |
;;+------------------+

(use-package doom-modeline
  :straight (doom-modeline :type git :host github :repo "seagle0128/doom-modeline")
  :init (defvar outline-minor-mode-prefix "\M-#")
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  ;; (set-face-attribute 'doom-modeline nil :family "Fira Code" :height 100
  ;; ;;                    'doom-modeline-inactive nil :family "Fira Code" :height 100)
  ;; 		      )
  (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange"))
;;(menu-bar-mode -1)



;;+---------------+
;;|   UI: Rules   |
;;+---------------+

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
     '(
       ;; (compilation-mode :custom rk/open-compilation-buffer :select t)
       ;;       ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
       ("\\*magit" :regexp t :same t :select t)
       ("*rustic-compilation*" :select t :align 'below :size 0.3)
       ("*cargo-run*" :select t :align 'below :size 0.3)
       ("\\*cargo-*" :regexp t :select t :align 'below :size 0.3)
       ("*rustfmt*" :select t :align 'below :size 0.3)
       ("\\*vterm*" :regexp t :same nil :other t :select t :align 'below :size 0.33)
       ("*vterm*" :same nil :other t :select t :align 'below :size 0.33)
       ;;       ("\\*PowerShell.*" :regexp t :same t :select t)
       ;;       ("\\*Cargo.*" :regexp t :other t :select nil)
       ("*Messages*" :select nil :other t)
       ;;       ("*Proced*" :select t :same t)
       ("*Buffer List*" :select t :same t)
       ;;       ("\\*Pp Eval" :regexp t :same nil :select t :other t) 
       ;;       ("*Messages*" :same nil :other t :select t :inhibit-window-quit t)
       ("*Help*" :other t :select nil :same nil :align 'below :size 0.5)
       ;; slime
       ("\\*sly-mrepl*" :reqxp t :select t :align 'below :size 0.3 )
       ("*slime-source*" :select nil :same nil :other t)
       ("*slime-description*" :select nil :other t :inhibit-window-quit t)
       ("\\*slime-repl" :regexp t :same nil :select nil :other t)
       ;; ("\\*sldb" :regexp t :other t :inhibit-window-quit t :select t)
       ("\\*slime-compilation" :regexp t :same nil :select nil :other t)
       ("*slime-scratch*" :same nil :select t :other t))))
   (shackle-default-rule nil))
  :config (shackle-mode))

(setq tab-always-indent 'complete)     ; Enable indentation+completion using the TAB key.


;;Line Numbering ;; Disable line numbers for some modes
(global-display-line-numbers-mode t)
(dolist (mode '(text-mode-hook
		org-mode-hook
		dievish-mode
		;; vundo-1
		sly-mrepl-mode-hook
                term-mode-hook
		vterm-mode-hook
		compilation
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))






(setq wwi-subject-alist '(("VFX Theory - II"         "FMAV 2101")
			  ("VFX Theory - III"        "FMAV 2102")
			  ("VFX Practical - II"      "FMAV 2103")
			  ("Night Shoot Project"     "FMAV 2104")
			  ("Filmmaking Basics I"     "FMBG 1101")
			  ("Basic Compositing"       "FMBV 2107")
			  ("Introduction to Houdini" "ANB3 2109")
			  ("Dynamics in Houdini"     "ANB3 2110")
			  ) )



;; (defun wwi-commands ()
;;   "A wwi related addon"
;;   (interactive)
;;   ((defvar wwi-subjects-get-choice (let (completing-read "My Prompt: " '("VFX Theory - II"
;; 									 "VFX Theory - III"
;; 									 "VFX Practical - II"
;; 									 "Night Shoot Project"
;; 									 "Filmmaking Basics I"
;; 									 "Basic Compositing"
;; 									 "Introduction to Houdini"
;; 									 "Dynamics in Houdini"
;; 									 ) nil nil))))
;;   (message (asscoc current-wwi-selected current-wwi-selected))
;;   )

;; (use-package wwi-commands
;;   :straight (wwi-commands :type nil
;; 			  :local-repo "~/.cache/emacs/scripts/el_wwi"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce"
     "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2"
     "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef"
     "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644"
     "a137529f2b83537fda674b1723043f0e5fcac21f9d0b5ea4aa64443c281bac2b"
     "f0eb51d80f73b247eb03ab216f94e9f86177863fb7e48b44aacaddbfe3357cf1"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

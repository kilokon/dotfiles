
(delete-selection-mode +1)
(tool-bar-mode -1)                    ; Disable toolbar
(tooltip-mode -1)                     ; Disable tooltips
(menu-bar-mode -1)                    ; Disable menu bar
(scroll-bar-mode -1)                  ; Disable scroll bar
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)


(setq gc-cons-threshold 100000000   ;; Set to (100mb)
      read-process-output-max (* 1024 1024) ;; 1mb
      auto-save-list-file-prefix nil ; Disable auto-save
      auto-mode-case-fold nil ; Use case-sensitive `auto-mode-alist' for performance
      command-line-x-option-alist nil ; Remove irreleant command line options for faster startup
      completion-cycle-threshold 3 ; TAB cycle if there are only few candidates
      create-lockfiles nil ; Disable lockfiles
      custom-file (concat user-emacs-directory "custom.el") ; Place all "custom" code in a temporary file
      display-line-numbers-type 'relative
      debug-on-error t
      debug-on-signal nil
      debug-on-quit nil
      enable-recursive-minibuffers t    ; Enable recursive minibuffers
      fast-but-imprecise-scrolling t ; More performant rapid scrolling over unfontified regions
      ffap-machine-p-known 'reject ; Don't ping things that look like domain names
      frame-inhibit-implied-resize t ; Inhibit frame resizing for performance
      inhibit-startup-message t ; Reduce noise at startup
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-screen t
      inhibit-default-init t
      initial-scratch-message nil
      idle-update-delay 1.0  ; slow down UI updates down
      inhibit-compacting-font-caches t ; Inhibit frame resizing for performance
      make-backup-files nil ; Disable backup files
      read-extended-command-predicate #'command-completion-default-include-p ; Corfu commands are hidden, since they are not supposed to be used via M-x.
      read-process-output-max (* 1024 1024) ; Increase how much is read from processes in a single chunk.
      redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
      select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings.
      tab-always-indent 'complete ; Enable indentation+completion using the TAB key.
      undo-limit 67108864 ; 64mb.
      undo-strong-limit 100663296 ; 96mb.
      undo-outer-limit 1006632960 ; 960mb.
      use-short-answers t ; y/n for yes/no
      vc-follow-symlinks t ; Do not ask about symlink following
      )


;; Autosave Files to local/share
(let ((saves-directory "~/.local/share/emacs/saves/"))
  (unless (file-directory-p saves-directory)
    (make-directory saves-directory))
  (setq auto-save-file-name-transforms
                `((".*" ,saves-directory t))))

;; Auto Backups to local/share
(let ((backups-directory "~/.local/share/emacs/backups"))
  (unless (file-directory-p backups-directory)
    (make-directory backups-directory))
  (setq backup-directory-alist `(("." . ,backups-directory)))
    (setq backup-by-copying t))

(load-theme 'modus-vivendi t)

(package-vc-install "https://github.com/emacs-evil/evil")

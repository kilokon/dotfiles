(if (eq nil window-system)
    (message "Emacs is running in terminal mode")
  (message "Emacs is running in graphical mode"))


(setq
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
 gc-cons-threshold 134217728
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

(delete-selection-mode +1)
(tool-bar-mode -1)                    ; Disable toolbar
(tooltip-mode -1)                     ; Disable tooltips
(menu-bar-mode -1)                    ; Disable menu bar
(scroll-bar-mode -1)                  ; Disable scroll bar
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
;; Global Hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Auto Cache All the icons
(let ((cache (expand-file-name
              "all-the-icons-font-installed" user-emacs-directory)))
  (unless (file-exists-p cache)
    (all-the-icons-install-fonts t)
    (with-temp-buffer (write-file cache))))


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


;; Add prompt indicator to `completing-read-multiple'.
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



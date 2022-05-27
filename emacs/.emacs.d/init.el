;;Aviik's Config
;;--------------

;;PERSONAL INFO
;;-------------

;;OS related
;;----------

;;Available Servers
;;-----------------

;;+-----------------------+
;;| Backup File Management|
;;+-----------------------+
;https://www.emacswiki.org/emacs/backup-each-save.el
;;(load "~/.emacs.d/scripts")
(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;;Default Settings
;;----------------
(setq visible-bell 1                    ; Annoying Ding off
      inhibit-startup-screen t
      initial-major-mode 'org-mode
      debug-on-error t                  ; Debug
      edebug-all-forms t                ; Debug
      undo-limit 200000                 ; Undo Limits
      undo-strong-limit 40000000        ; Undo Limits
      load-prefer-newer t               ; Load Newer el files
      make-pointer-invisible t         ; No Mouse hide while editing
)

;;Auto-Save Management
;;--------------------


;;Package Management
;;------------------
;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)
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


;;(use-package all-the-icons
;;             :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el"))

;; (use-package yasnippet
;;              :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet"))

;;https://github.com/coldnew/coldnew-emacs/blob/master/init.org

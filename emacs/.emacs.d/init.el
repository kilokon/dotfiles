;; -*- lexical-binding: t -*-
;; -*- coding:utf-8 -*-
;;+-----------------------------------+
;;|   `UI': 'Global-Font-Settings'    |
;;+-----------------------------------+
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :weight 'light :height 100)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :weight 'light :height 120)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.1)
(setq-default line-spacing 0)
;; (setq x-underline-at-descent-line t)

(setq widget-image-enable nil)
(set-default 'cursor-type  '(bar . 1))  ;;; Line cursor type
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)

(defun load-custom (n)
  (time (format "load %s" n)
    (load n nil 'no-message)))

(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

; (load-file (expand-file-name "quelpa-plugins.el" user-emacs-directory))


(defun load-package-configs (filenames)
  "Load the specified Emacs Lisp files from the `elisp_dir` subdirectory of `user-emacs-directory`."
  (dolist (filename filenames)
    (let* ((elisp-dir (expand-file-name "elisp" user-emacs-directory))
           (filepath (expand-file-name (concat filename ".el") elisp-dir)))
      (when (file-exists-p filepath)
        (message "Found %s" filepath)
        (load-file filepath)))))

(load-package-configs '(
                        ; "default_packages"
                        "emacs_defaults"
                        "theme_settings"
                        "plugins"
                        ; "dired_settings"
                        ; "org_settings"
                        ; "vim_settings"
                        ; "common_packages"
                        ; "version_control"
                        ; "vertico_settings"
                        ; "marginalia_settings"
                        ; "consult_settings"
                        ; "embark_settings"
                        ; "snippet_settings"
                        ; "completion_settings"
                        ; "terminal_related"
                        ; "language_server"
                        ; "rust_settings"
                        ; "latex_settings"
                        ))



;; (require 'vim_settings)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; (global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-x") 'execute-extended-command)

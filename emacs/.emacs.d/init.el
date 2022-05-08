(setq visible-bell 1                    ; Annoying Ding off
      debug-on-error t                  ; Debug
      edebug-all-forms t                ; Debug
      undo-limit 200000                 ; Undo Limits
      undo-strong-limit 40000000        ; Undo Limits
      load-prefer-newer t               ; Load Newer el files
      make-pointer-invisible t         ; No Mouse hide while editing
      initial-major-mode 'org-mode)


(setq-default inhibit-startup-screen t)

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



(use-package general
             :straight (general :type git :host github :repo "noctuid/general"))


(use-package all-the-icons
             :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el"))















(use-package yasnippet
             :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet"))

;;https://github.com/coldnew/coldnew-emacs/blob/master/init.org

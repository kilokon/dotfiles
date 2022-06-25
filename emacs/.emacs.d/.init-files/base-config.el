(message "base config loading.......")

(savehist-mode)
(recentf-mode t)


;;Default Settings
;;----------------
(setq visible-bell 1                    ; Annoying Ding off
      ring-bell-function 'ignore
      inhibit-startup-screen t
      initial-major-mode 'org-mode
      debug-on-error t                  ; Debug
      edebug-all-forms t                ; Debug
      undo-limit 200000                 ; Undo Limits
      undo-strong-limit 40000000        ; Undo Limits
      load-prefer-newer t               ; Load Newer el files
      make-pointer-invisible t          ; Hide Mouse While editing
      package-enable-at-startup nil     ; Disable package.el in favor of straight.el
      enable-recursive-minibuffers t    ; Enable recursive minibuffers
      )







(message "base config loaded......")

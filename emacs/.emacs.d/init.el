

;;PRIORITY UNO
(defun aviikc/reload-init-file()
 "It reloads Emacs init config."
 (interactive)
 (load-file user-init-file))


(defconst user-init-dir
  (expand-file-name ".init-files" user-emacs-directory))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))


(load-user-file "base-config.el")





(global-set-key (kbd "C-c r") 'aviikc/reload-init-file)

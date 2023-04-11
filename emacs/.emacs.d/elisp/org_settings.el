;;(message "%s" org-version)
;; enhanced LaTeX mode
(setq org-drive-map  (list (cons 'agenda "planner")
                           (cons 'todo "todos")
                           (cons 'notes "Notes")))




(use-package markdown-mode
  :hook (markdown-mode . auto-fill-mode)
  :custom-face (markdown-code-face ((t (:inherit org-block)))))

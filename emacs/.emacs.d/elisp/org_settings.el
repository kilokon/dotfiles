;;(message "%s" org-version)
;; enhanced LaTeX mode
(setq org-drive-map  (list (cons 'agenda "planner")
                           (cons 'todo "todos")
                           (cons 'notes "Notes")))




(use-package markdown-mode
  :hook (markdown-mode . auto-fill-mode)
  :custom-face (markdown-code-face ((t (:inherit org-block)))))



(require 'ox-reveal)



(setq ob-mermaid-cli-path "/home/aviik/.nvm/versions/node/v18.15.0/bin/mmdc")


(org-babel-do-load-languages
    'org-babel-load-languages
    '((mermaid . t)
      (scheme . t)))

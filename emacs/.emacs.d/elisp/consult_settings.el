;; -*- lexical-binding: t -*-

(use-package consult
  :after vertico
  :bind (("C-s" . consult-line)
	 ("M-y" . consult-yank-pop))
  :custom (completion-in-region-function #'consult-completion-in-region))


;; fzf provider for consult
(use-package affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package consult-dir
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

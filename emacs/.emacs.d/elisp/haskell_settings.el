;;(require 'haskell-mode-autoloads)

;; (use-package haskell-mode-autoloads
;;   :load-path "~/.emacs.d/quelpa/build/haskell-mode/"
;;   :hook ((haskell-mode . origami-mode)
;;          ;; (haskell-mode . company-mode)
;;          (haskell-mode . haskell-indentation-mode)
;;          ;; this enables better integration with the running GHCi process
;;          ;; NOTE this is NOT the same is haskell-interactive-mode which is used
;;          ;; in the repl that is launched within projects when loading files
;;          (haskell-mode . interactive-haskell-mode)
;;          (haskell-mode . flymake-mode)
;;          (haskell-mode .(lambda ()
;;                           (set (make-local-variable 'company-backends)
;;                                (append '((company-capf company-dabbrev-code))
;;                                        company-backends))))
;;          ;; camelcase is defacto for haskell
;;          (haskell-mode . subword-mode))
;;   :config (setq haskell-stylish-on-save t
;;                 haskell-interactive-popup-errors nil
;;           ;; we use stack...which counterintuitively means we set the
;;           ;; cabal build command to be stack
;;                 haskell-compile-cabal-build-command "stack build"
;;           ;; use stylish (requires the stylish binary somewhere in $PATH)
;;           ;; use some handy suggestions
;;                 haskell-process-suggest-remove-import-lines t
;;                 haskell-process-auto-import-loaded-modules t
;;                 ;; use TAGS file (requires hasktags binary to be in $PATH)
;;                 haskell-tags-on-save t))


(add-to-list 'load-path "~/.emacs.d/quelpa/build/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/quelpa/build/haskell-mode/")


(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-cabal-mode 'subword-mode)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; (add-auto-mode 'haskell-mode "\\.ghci\\'")

  ;; Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  (append '((company-capf company-dabbrev-code))
;;                          company-backends))))
;; (add-hook 'haskell-interactive-mode (lambda () (setq haskell-process-show-debug-tips nil)))

(custom-set-variables '(haskell-process-show-debug-tips nil)
                      '(haskell-process-auto-import-loaded-modules t))


(add-hook 'haskell-mode-hook
          (defun my/haskell-hook ()
            (require 'attrap)
            (require 'flymake-flycheck)
            (flymake-mode)
            (add-hook 'flymake-diagnostic-functions 'attrap-flymake-hlint nil t)))
  ;; Source code helpers

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  ;;(when (require 'dante-mode))
  ;; (when (maybe-require-package 'reformatter)
  ;;   (reformatter-define hindent
  ;;     :program "hindent"
  ;;     :lighter " Hin")

  ;;   (defalias 'hindent-mode 'hindent-on-save-mode)

  ;;   (reformatter-define ormolu
  ;;     :program "ormolu"
  ;;     :lighter " Orm"))

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))


(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'haskell-mode))


(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :hook
  (haskell-mode . dante-mode)
  ;; :config
  ;; (require 'flymake-flycheck)
  ;; (defalias 'flymake-hlint
    ;; (flymake-flycheck-diagnostic-function-for 'haskell-hlint))
  ;; (add-to-list 'flymake-diagnostic-functions 'flymake-hlint)
  )

;; (use-package attrap)
(use-package attrap
  :ensure t
  :bind (("C-x /" . attrap-attrap)))



(use-package ormolu
 :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
   ("C-c r" . ormolu-format-buffer)))

(defun create-unfocused-frame ()
  (let*
    ((prv (window-frame))
     (created (make-frame)))
    (select-frame-set-input-focus prv) created))

(defun create-haskell-interactive-frame ()
  (interactive)
  (haskell-interactive-bring)
  (create-unfocused-frame)
  (delete-window))

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; (use-package orderless
;;   :commands (orderless)
;;   :init
;;   (setq completion-styles '(orderless partial-completion basic)
;;         completion-category-defaults nil
;;         completion-category-overrides nil
;;         completion-ignore-case t))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (completion-styles '(orderless-fast))
  ;; (corfu-popupinfo-delay '(0.5 . 0.2))
  ;; (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first nil)
  ;; (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))

  :init
  (global-corfu-mode)
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode) ; Popup completion info
  ;; (setq user)
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))


(use-package dabbrev
    ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; (use-package cape
;;   :defer 10
;;   :bind ("C-c f" . cape-file)
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
;;   (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
;;   (cl-pushnew #'cape-file completion-at-point-functions)
;;   :config
;;   ;; Silence then pcomplete capf, no errors or messages!
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;;   ;; Ensure that pcomplete does not write to the buffer
;;   ;; and behaves as a pure `completion-at-point-function'.
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))


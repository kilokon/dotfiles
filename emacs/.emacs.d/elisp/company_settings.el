;; (use-package company
;;   :ensure t
;;   :config
;; (setq company-idle-delay 0.1
;;       company-minimum-prefix-length 2
;;       company-selection-wrap-around t
;;       company-show-numbers t
;;       company-require-match 'never
;;       company-dabbrev-downcase nil
;;       company-dabbrev-ignore-case t
;;       company-backends '( company-capf
;;                                       (company-dabbrev-code company-keywords)
;;                                       company-files company-dabbrev)))
;; (global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
;; (with-eval-after-load 'company
;;   (define-key company-active-map
;;               (kbd "TAB")
;;               #'company-complete-common-or-cycle)
;;   (define-key company-active-map
;;               (kbd "<backtab>")
;;               (lambda ()
;;                 (interactive)
;;                 (company-complete-common-or-cycle -1))))
;; Add extensions

(defvar emojis
  '((":-D" . "😀")
    (";-)" . "😉")
    (":-/" . "😕")
    (":-(" . "🙁")
    (":-*" . "😙")))

(defun emoji-backend (action &optional arg &rest _)
  (pcase action
    ('prefix (and (memq (char-before) '(?: ?\;))
                  (cons (string (char-before)) t)))
    ('candidates (all-completions arg emojis))
    ('annotation (concat " " (cdr (assoc arg emojis))))
    ('post-completion
     (let ((str (buffer-substring (- (point) 3) (point))))
       (delete-region (- (point) 3) (point))
     (insert (cdr (assoc str emojis)))))))

;; Register emoji backend with `completion-at-point'
(setq completion-at-point-functions
      (list (cape-company-to-capf #'emoji-backend)))

;; Register emoji backend with Company.
;; (setq company-backends '(emoji-backend))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  ;; :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'cmpletion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)


(use-package company
  :ensure t
  :diminish (company-mode  .  " Ⓒ ")
  :bind (:map company-active-map
         ("TAB" . company-select-next)
         ("<backtab>" . company-select-previous))
  :init
  (global-company-mode)
;;  :hook ((company-mode . company-prescient-mode))
  :config
  ;; set default company-backends
  (setq company-idle-delay 0.1
        lsp-completion-provider :capf
        company-echo-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-backends '( company-capf
                                      (company-dabbrev-code company-keywords)
                                      company-files company-dabbrev emoji-backend)))

;; Use Company backends as Capfs.
(setq-local completion-at-point-functions
  (mapcar #'cape-company-to-capf
          (list #'company-files #'company-ispell #'company-dabbrev)))

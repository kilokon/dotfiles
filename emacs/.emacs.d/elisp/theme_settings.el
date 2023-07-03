;; (use-package all-the-icons
;;     :ensure t)

;; Auto Cache All the icons
;; (let ((cache (expand-file-name
;;               "all-the-icons-font-installed" user-emacs-directory)))
;;   (unless (file-exists-p cache)
;;     (all-the-icons-install-fonts t)
;;     (with-temp-buffer (write-file cache))))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1))

;;+-----------------------------------+
;;|   `UI': 'Global-Font-Settings'    |
;;+-----------------------------------+
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :weight 'light :height 100)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :weight 'light :height 120)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.1)
(setq-default line-spacing 0)
;; (setq x-underline-at-descent-line t)

;; (setq widget-image-enable nil)
(use-package simple-modeline
  :init (simple-modeline-mode)
  :custom
  (simple-modeline-segments
   '((simple-modeline-segment-modified
      simple-modeline-segment-buffer-name
      simple-modeline-segment-position)
     (
      ;; simple-modeline-segment-minor-modes
      simple-modeline-segment-vc
      simple-modeline-segment-misc-info
      simple-modeline-segment-process
      simple-modeline-segment-major-mode)))
  :custom-face
  (simple-modeline-space ((t (:box nil)))))

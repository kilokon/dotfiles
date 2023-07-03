;; (use-package tex
  ;; :ensure auctex
;;   :config
;;   (setq TeX-PDF-mode t)
;;   (setq TeX-parse-self t)
;;   (setq TeX-auto-save t)
;;   (setq TeX-save-query nil)
;;   (add-hook 'doc-view-mode-hook 'auto-revert-mode)
;;   (add-hook 'TeX-mode-hook
;;             '(lambda ()
;;                (define-key TeX-mode-map (kbd "<C-f8>")
;;                  (lambda ()
;;                    (interactive)
;;                    (TeX-command-menu "LaTeX"))))))


;; (use-package auctex
;;   :ensure t
;;   :defer t
;;   :hook (LaTeX-mode . (lambda ()
;; 			(push (list 'output-pdf "Zathura")
;; 			      TeX-view-program-selection))))
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)
;; (setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)

;; (add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.
;; (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode); Enable Flyspell program mode for emacs lisp mode, which highlights all misspelled words in comments and strings.
;; (setq ispell-dictionary "english"); Default dictionary. To change do M-x ispell-change-dictionary RET.
;; (add-hook 'TeX-mode-hook
;;           (lambda () (TeX-fold-mode 1))); Automatically activate TeX-fold-mode.
;; (setq LaTeX-babel-hyphen nil); Disable language-specific hyphen insertion.


;; ;; " expands into csquotes macros (for this to work babel must be loaded after csquotes).
;; (setq LaTeX-csquotes-close-quote "}"
;;       LaTeX-csquotes-open-quote "\\enquote{")

;; ;; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
;; (add-hook 'TeX-mode-hook 'LaTeX-math-mode)


;; ;;; RefTeX
;; ;; Turn on RefTeX for AUCTeX http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
;; (add-hook 'TeX-mode-hook 'turn-on-reftex)
(use-package tex
  :ensure auctex
  :hook
  ((LaTeX-mode . flycheck-mode)
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . fci-mode)
   (LaTeX-mode . reftex-mode)
   (LaTeX-mode . (lambda ()
			(push (list 'output-pdf "Zathura")
			      TeX-view-program-selection)))
   ;; sync tex buffer positions to output pdf
   (LaTeX-mode . TeX-source-correlate-mode))
  :config
  (turn-on-reftex)
  (setq TeX-after-compilation-finished-functions '(TeX-revert-document-buffer)
        reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Default")))

;; add cleveref support
(with-eval-after-load 'latex
  (TeX-add-style-hook
   "cleveref"
   (lambda ()
     (when (boundp 'reftex-ref-style-alist)
       (add-to-list 'reftex-ref-style-alist
                    '("Cleveref" "cleveref" (("\\cref" ?c)
                                             ("\\Cref" ?C)
                                             ("\\cpageref" ?d)
                                             ("\\Cpageref" ?D)))))
     (reftex-ref-style-activate "Cleveref")
     (TeX-add-symbols
      '("cref" TeX-arg-ref)
      '("Cref" TeX-arg-ref)
      '("cpageref" TeX-arg-ref)
      '("Cpageref" TeX-arg-ref)))))




(eval-after-load 'reftex-vars; Is this construct really needed?
  '(progn
     (setq reftex-cite-prompt-optional-args t); Prompt for empty optional arguments in cite macros.
     ;; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
     (setq reftex-plug-into-AUCTeX t)
     ;; So that RefTeX also recognizes \addbibresource. Note that you
     ;; can't use $HOME in path for \addbibresource but that "~"
     ;; works.
     (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
;     (setq reftex-default-bibliography '("UNCOMMENT LINE AND INSERT PATH TO YOUR BIBLIOGRAPHY HERE")); So that RefTeX in Org-mode knows bibliography
     (setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]"); Recognize \subcaptions, e.g. reftex-citation
     (setq reftex-cite-format; Get ReTeX with biblatex, see https://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
           '((?t . "\\textcite[]{%l}")
             (?a . "\\autocite[]{%l}")
             (?c . "\\cite[]{%l}")
             (?s . "\\smartcite[]{%l}")
             (?f . "\\footcite[]{%l}")
             (?n . "\\nocite{%l}")
             (?b . "\\blockcquote[]{%l}{}")))))

;; Fontification (remove unnecessary entries as you notice them) http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00236.html http://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html
(setq font-latex-match-reference-keywords
      '(
        ;; biblatex
        ("printbibliography" "[{")
        ("addbibresource" "[{")
        ;; Standard commands
        ;; ("cite" "[{")
        ("Cite" "[{")
        ("parencite" "[{")
        ("Parencite" "[{")
        ("footcite" "[{")
        ("footcitetext" "[{")
        ;; ;; Style-specific commands
        ("textcite" "[{")
        ("Textcite" "[{")
        ("smartcite" "[{")
        ("Smartcite" "[{")
        ("cite*" "[{")
        ("parencite*" "[{")
        ("supercite" "[{")
        ; Qualified citation lists
        ("cites" "[{")
        ("Cites" "[{")
        ("parencites" "[{")
        ("Parencites" "[{")
        ("footcites" "[{")
        ("footcitetexts" "[{")
        ("smartcites" "[{")
        ("Smartcites" "[{")
        ("textcites" "[{")
        ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands
        ("autocite" "[{")
        ("Autocite" "[{")
        ("autocite*" "[{")
        ("Autocite*" "[{")
        ("autocites" "[{")
        ("Autocites" "[{")
        ;; Text commands
        ("citeauthor" "[{")
        ("Citeauthor" "[{")
        ("citetitle" "[{")
        ("citetitle*" "[{")
        ("citeyear" "[{")
        ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands
        ("fullcite" "[{")))

(setq font-latex-match-textual-keywords
      '(
        ;; biblatex brackets
        ("parentext" "{")
        ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary Commands
        ("textelp" "{")
        ("textelp*" "{")
        ("textins" "{")
        ("textins*" "{")
        ;; supcaption
        ("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(
        ;; amsmath
        ("numberwithin" "{")
        ;; enumitem
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")))




(defun nd/init-company-auctex ()
  "Set the company backends for auctex modes."
  (setq-local company-backends '((company-auctex-labels
                                  company-auctex-bibs
                                  company-auctex-macros
                                  company-auctex-symbols
                                  company-auctex-environments
                                  ;; company-latex-commands
                                  company-math-symbols-latex
                                  company-math-symbols-unicode))))

;; (use-package company-math
;;   :after (tex company)
;;   :config
;;   (setq company-math-allow-unicode-symbols-in-faces '(font-latex-math-face)
;;         company-math-disallow-latex-symbols-in-faces nil))

;; (use-package company-auctex
;;   :after (tex company company-math)
;;   :hook
;;   ((LaTeX-mode . nd/init-company-auctex)))

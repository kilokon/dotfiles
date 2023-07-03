;;; initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))


(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))


(setq quelpa-update-melpa-p t)

(unless (package-installed-p 'polymode)
  (package-install 'poly-markdown)
 )

;; use-package system install and initiate
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"
   :stable nil))
(require 'quelpa-use-package)
(quelpa '(evil :fetcher github :repo "emacs-evil/evil"))


;;essential packages that need to be compiled beforE
(quelpa '(evil-terminal-cursor-changer :fetcher github :repo "7696122/evil-terminal-cursor-changer"))
;;(quelpa '(evil-escape :fetcher github :repo "syl20bnr/evil-escape"))
(quelpa '(vterm :fetcher github :repo "akermu/emacs-libvterm"))
(quelpa '(compat :fetcher github :repo "emacs-compat/compat"))
(quelpa '(spinner :fetcher github :repo "Malabarba/spinner.el"))
(quelpa '(orderless :fetcher github :repo "oantolin/orderless"))
(quelpa 'all-the-icons)
(quelpa 'tree-sitter)
(quelpa 'tree-sitter-langs)
(quelpa '(simple-modeline :fetcher github :repo "gexplorer/simple-modeline"))
(quelpa 'company-mode)
;; Haskell Mode
(quelpa '(haskell-mode :fetcher github :repo "haskell/haskell-mode"))
(quelpa '(dante :fetcher github :repo "jyp/dante"))
(quelpa '(attrap :fetcher github :repo "jyp/attrap"))
(quelpa '(ormolu :fetcher github :repo "vyorkin/ormolu.el"))
(quelpa '(reformatter :fetcher github :repo "purcell/emacs-reformatter"))
(quelpa 'auctex)
(quelpa '(company-auctex :fetcher github :repo "alexeyr/company-auctex"))
(quelpa '(company-math :fetcher github :repo "vspinu/company-math"))
(quelpa '(flymake-flycheck :fetcher github :repo "purcell/flymake-flycheck"))
(quelpa '(ox-reveal :fetcher github :repo "yjwen/org-reveal"))
(quelpa '(ob-mermaid :fetcher github :repo "arnm/ob-mermaid"))
(quelpa '(typst-mode :fetcher github :repo "Ziqi-Yang/typst-mode.el"))


(setq my-package-list '((affe              :fetcher github :repo "minad/affe")

                        ;; (cape              :fetcher github :repo "minad/cape")
                        ;; Consult provides search and navigation commands based on the Emacs completion
                        (consult           :fetcher github :repo "minad/consult")
                        (consult-dir       :fetcher github :repo "karthink/consult-dir")
                        (consult-lsp       :fetcher github :repo "gagbo/consult-lsp")
                        ;; (corfu             :fetcher github :repo "minad/corfu")
                        (doom-themes)
                      ;;  (doom-modeline)
                        (dap-mode)
                        (diminish          :fetcher github :repo "myrjola/diminish.el")
                        (dirvish           :fetcher github :repo "alexluigit/dirvish")
                        ;; fzf provider for consult
                        (embark            :fetcher github :repo "oantolin/embark")
                        ;; evil-commentary is an Emacs package for evil-mode that intends to
                        ;; make it easy to comment out (lines of) code
                        (evil-commentary   :fetcher github :repo "linktohack/evil-commentary")
                        (evil-collection)
                        (exec-path-from-shell)
                        (fancy-dabbrev     :fetcher github :repo "jrosdahl/fancy-dabbrev")
                        (flycheck)
                        (general           :fetcher github :repo "noctuid/general.el")
                        (linum-relative    :fetcher github :repo "coldnew/linum-relative")
                        (lsp-mode)
                        (lsp-ui)
                        (magit)
                        (magit-rockstar)
                        (marginalia        :fetcher github :repo "minad/marginalia")
                        (multiple-cursors  :fetcher github :repo "magnars/multiple-cursors.el")

                        (org)
                        (popup             :fetcher github :repo "auto-complete/popup-el")
                        (pulsar            :fetcher github :repo "protesilaos/pulsar")
                        (rustic            :fetcher github :repo "brotzeit/rustic")
                        (smartparens       :fetcher github :repo "Fuco1/smartparens")
                        (smart-mode-line)
                        (tempel            :fetcher github :repo "minad/tempel")
                        (tempel-collection :fetcher github :repo "Crandel/tempel-collection")
                        (undo-fu           :fetcher github :repo "emacsmirror/undo-fu")
                        (vertico           :fetcher github :repo "minad/vertico" :files ("*.el" "extensions/*.el"))
                        (vterm-toggle      :fetcher github :repo "jixiuf/vterm-toggle")
                        ;; which-key is a minor mode for Emacs that displays the key bindings
                        (which-key         :fetcher github :repo "justbur/emacs-which-key")
                        ))

(dolist (package my-package-list)
  (quelpa package))

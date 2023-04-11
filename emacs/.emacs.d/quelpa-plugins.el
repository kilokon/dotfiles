;;; initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))


(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))


(setq quelpa-update-melpa-p t)

;; use-package system install and initiate
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"
   :stable nil))
(require 'quelpa-use-package)



(setq my-package-list '((affe              :fetcher github :repo "minad/affe")                   
                        (all-the-icons     :fetcher github :repo "domtronn/all-the-icons.el")
                        (auctex)
                        (cape              :fetcher github :repo "minad/cape")
                        (compat            :fetcher github :repo "emacs-compat/compat")
                        ;; Consult provides search and navigation commands based on the Emacs completion
                        (consult           :fetcher github :repo "minad/consult")
                        (consult-dir       :fetcher github :repo "karthink/consult-dir")
                        (consult-lsp       :fetcher github :repo "gagbo/consult-lsp")
                        (corfu             :fetcher github :repo "minad/corfu")
                        (dap-mode)
                        (diminish          :fetcher github :repo "myrjola/diminish.el")
                        (dirvish           :fetcher github :repo "alexluigit/dirvish")
                        ;; fzf provider for consult
                        (embark            :fetcher github :repo "oantolin/embark")
                        (evil              :fetcher github :repo "emacs-evil/evil")
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
                        (orderless         :fetcher github :repo "oantolin/orderless")
                        (org)
                        (popup             :fetcher github :repo "auto-complete/popup-el")
                        (pulsar            :fetcher github :repo "protesilaos/pulsar")
                        (rustic            :fetcher github :repo "brotzeit/rustic")
                        (smartparens       :fetcher github :repo "Fuco1/smartparens")
                        (smart-mode-line)
                        (spinner           :fetcher github :repo "Malabarba/spinner.el")
                        (tempel            :fetcher github :repo "minad/tempel")
                        (tempel-collection :fetcher github :repo "Crandel/tempel-collection")
                        (undo-fu           :fetcher github :repo "emacsmirror/undo-fu")
                        (vertico           :fetcher github :repo "minad/vertico" :files ("*.el" "extensions/*.el"))
                        (vterm             :fetcher github :repo "akermu/emacs-libvterm")
                        (vterm-toggle      :fetcher github :repo "jixiuf/vterm-toggle")
                        ;; which-key is a minor mode for Emacs that displays the key bindings
                        (which-key         :fetcher github :repo "justbur/emacs-which-key")
                        ))

(dolist (package my-package-list)
  (quelpa package))

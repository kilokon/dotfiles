;; -*- lexical-binding: t -*-
;; -*- coding:utf-8 -*-

;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org
;; The default is 800 kilobytes.  Measured in bytes.
(defvar best-gc-cons-threshold (* 50 1000 1000)
  "Best default gc threshold value in bytes.  Should NOT be too big!")


(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 1)

;; Profile emacs startup
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "*** Emacs loaded in %s seconds with %d garbage collections."
    (emacs-init-time "%.2f") gcs-done)))

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda ()
     (unless (frame-focus-state)
       (garbage-collect)))))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (expand-file-name (format "emacs%d" (user-uid))
                    temporary-file-directory))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

(setq-default indent-tabs-mode nil)


(setq-default
 default-frame-alist
 '((right-divider-width . 1) ;; Thin vertical window divider
   (right-fringe . 8))) ;; Thin right fringe

;; from: https://github.com/SystemCrafters/rational-emacs/blob/master/early-init.el
;; native compilation settings
(when (featurep 'native-compile)
  ;; silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t))


(add-hook
 'emacs-startup-hook
 (lambda ()
   (when (get-buffer "*scratch*")
     (kill-buffer "*scratch*"))))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; (recentf-mode 1)
;; Remember and restore the last cursor location of opened files
;; (save-place-mode 1)
(setq
 debug-on-error t
 package-enable-at-startup nil)

(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes 0))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

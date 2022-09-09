;; Increases Garbage Collection During Startup
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-repository-branch "develop")

(use-package emacs
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (add-to-list 'default-frame-alist '(font . "MonoLisa 14"))
					; :hook(minibuffer-setup . (lambda () (set-input-method "programmer-dvorak")))
  :custom
  (vc-follow-symlinks t)
  (enable-recursive-minibuffers t))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))
(use-package programmer-dvorak)
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :hook(evil-insert-state-entry . (lambda () (set-input-method "programmer-dvorak")))
  :config(evil-mode 1))

(add-hook 'org-mode-hook 'visual-line-mode)
(use-package counsel
  :config(counsel-mode))

(use-package powerline)

(use-package magit)

(setq calendar-latitude +55)
(setq calendar-longitude +3)
(use-package moe-theme
  :requires powerline
  :config
  (require 'powerline)
  (powerline-moe-theme)
  (moe-dark))


(use-package pdf-tools
; :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install)
  (require 'pdf-occur))


(use-package restart-emacs
  :commands (restart-emacs)
  :bind ("C-c r" . restart-emacs))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))


(use-package page-break-lines)
(use-package all-the-icons)

(defun config ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "/init.el"))))
(bind-key "C-c f c" 'config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

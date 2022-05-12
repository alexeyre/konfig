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

(use-package counsel
  :config(counsel-mode))

(use-package powerline)

(use-package moe-theme
  :requires powerline
  :config
  (require 'powerline)
  (powerline-moe-theme)
  (moe-dark))

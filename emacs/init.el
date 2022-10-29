(setq inhibit-compacting-font-caches t)
(setq package-enable-at-startup nil)
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

(setq native-comp-async-report-warnings-errors nil)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-repository-branch "develop")

(global-auto-revert-mode t)
(add-to-list 'default-frame-alist '(undecorated-round . t))

(use-package dashboard

  :config
  (setq dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory))
  (dashboard-setup-startup-hook))

(use-package exec-path-from-shell
  :custom(exec-path-from-shell-arguments '("-l"))
  :init(exec-path-from-shell-initialize))

(use-package which-key
  :init(which-key-mode))

(use-package emacs
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (add-to-list 'default-frame-alist '(font . "MonoLisa 14"))
  :hook(minibuffer-setup . (lambda () (set-input-method "programmer-dvorak")))
  :custom
  (vc-follow-symlinks t)
  (enable-recursive-minibuffers t))


(use-package programmer-dvorak)

(use-package evil
  :hook
  (evil-insert-state-entry . (lambda () (set-input-method "programmer-dvorak")))
  (evil-replace-state-entry . (lambda () (set-input-method "programmer-dvorak")))
  (isearch-mode . (lambda () (set-input-method "programmer-dvorak")))
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package ivy-posframe
  :custom(ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  :config(ivy-posframe-mode))

(use-package counsel
  :config(counsel-mode)
  :bind
  ("M-x" . counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-c p" . counsel-fzf)
  ("C-x b" . counsel-switch-buffer))

(use-package company
  :bind (:map prog-mode-map
         ("C-i" . company-indent-or-complete-common)
         ("C-M-i" . counsel-company))
  :hook prog-mode)

(use-package markdown-mode+
  :mode ("\\.md\\'" . markdown-mode+))

(use-package magit
  :commands magit)
(use-package projectile
  :bind(:map projectile-mode-map
	     ("C-c C-p" . 'projectile-command-map))
  :custom
  (projectile-project-search-path (cddr (directory-files "~/Projects" t)))
  (projectile-completion-system 'ivy)
  :init(projectile-mode +1)
  )
(use-package evil-anzu
  :init(anzu-mode))
(use-package doom-themes
  :bind
  ("C-c t l" . 'light)
  ("C-c t d" . 'dark)
  :config
  (defun light ()
    (interactive)
    (progn
      (disable-theme 'doom-one)
      (enable-theme 'doom-one-light)
      ))
  (provide 'light)
  (defun dark ()
    (interactive)
    (progn
      (disable-theme 'doom-one-light)
      (enable-theme 'doom-one)
      ))
  (provide 'dark)
  (doom-themes-org-config)
  (load-theme 'doom-one-light t)
  (load-theme 'doom-one t)
  (dark))

(use-package doom-modeline
  :custom
  (doom-modeline-time t)
  :init(display-battery-mode)
  :hook(after-init . doom-modeline-mode))
(use-package cdlatex
  :hook
  (org-mode . turn-on-org-cdlatex))
(use-package org
  :straight nil
  :custom(org-preview-latex-image-directory "~/.local/cache/org-previews/")
  :config(require 'org-tempo)
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . (lambda ()
		     (interactive)
		     (setq buffer-face-mode-face '(:family "Space Mono"))
		     (buffer-face-mode))))
(use-package org-fragtog
  :hook(org-mode . org-fragtog-mode))
(bind-key "C-l" #'(lambda () ""
		    (interactive)
		    (find-file "~/Documents/knowledge/main.org")
		    (counsel-org-goto)))

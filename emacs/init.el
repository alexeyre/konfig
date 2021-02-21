; -*- lexical-binding: t; -*-
(if (version< emacs-version "27.0")
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))
(let ((file-name-handler-alist nil))
 (eval-when-compile
  (straight-use-package 'use-package))
 (straight-use-package 'diminish)                ;; if you use :diminish
 (straight-use-package 'bind-key)                ;; if you use any :bind variant
 ;; always straight a package by default
 (setq straight-use-package-by-default t)
 (straight-use-package 'no-littering)
 (straight-use-package 'use-package-ensure-system-package)
 (delete-file (expand-file-name "config.el" user-emacs-directory))
 (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))

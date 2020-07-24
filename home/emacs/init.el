; -*- lexical-binding: t; -*-
(if (version< emacs-version "27.0")
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))
(let ((file-name-handler-alist nil))
  (straight-use-package 'bind-key)
  (straight-use-package 'diminish)
  ;; always straight a package by default
  (setq straight-use-package-by-default t)
  (straight-use-package 'no-littering)
  (straight-use-package 'use-package-ensure-system-package)
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))

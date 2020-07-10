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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" default)))
 '(org-agenda-files
   (quote
    ("~/notes/20200619184615-index.org" "~/notes/notes.org")))
 '(safe-local-variable-values
   (quote
    ((eval alex/flowy-note)
     (olivetti-body-width . 110)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

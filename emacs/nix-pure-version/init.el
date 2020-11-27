; -*- lexical-binding: t; -*-
(if (version< emacs-version "27.0")
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))
(delete-file (expand-file-name "config.el" user-emacs-directory))
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

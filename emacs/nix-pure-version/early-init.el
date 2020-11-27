(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(with-no-warnings
  (require 'cl))
(setq vc-follow-symlinks t)

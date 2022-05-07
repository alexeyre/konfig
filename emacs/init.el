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


(use-package evil
             :config(evil-mode 1))

(use-package sis
             :config
             (sis-ism-lazyman-config
               "org.unknown.keylayout.ProgrammerDvorak"
               "com.apple.keylayout.US"
               )
             (sis-global-respect-mode t)
	     :hook
	     (evil-normal-state-entry . sis-set-other)
	     (evil-insert-state-entry . sis-set-english)
	     )
(use-package counsel
  :config(counsel-mode))

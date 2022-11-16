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

(straight-use-package 'leaf)

(defun dvorak (&rest ignored)
  (set-input-method "programmer-dvorak"))
(defcustom leaf-alias-keyword-alist '((:ensure . :straight))
  "The alias keyword.  KEY is treated as an alias for VALUE."
  :type 'sexp
  :group 'leaf)
(leaf leaf-keywords
  :ensure t
  :config
  (leaf-keywords-init))

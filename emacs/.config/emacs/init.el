(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq native-comp-async-report-warnings-errors nil)
(setq enable-recursive-minibuffers t)
(straight-use-package 'gcmh)
(require 'gcmh)
(gcmh-mode 1)
(setq vc-follow-symlinks t)
(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)
(setq straight-use-package-by-default t)
(setq vc-follow-symlinks t)
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package tree-sitter
  :config(global-tree-sitter-mode))
(use-package tree-sitter-langs
  :after tree-sitter)

;; transparency binds
;; https://www.emacswiki.org/emacs/TransparentEmacs
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ;; Also handle undocumented (<active> <inactive>) form.
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(90 . 90) '(100 . 100)))))
(toggle-transparency)
(bind-key "C-c t t" 'toggle-transparency)


;; lsp
(use-package lsp-mode
  :hook (lsp-configure . (lambda () (lsp-headerline-breadcrumb-mode -1)))
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
		      :major-modes '(c-mode c++-mode)
		      :remote? t
		      :server-id 'clangd-remote))))
(use-package lsp-ivy)
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  (latex-mode . yas-minor-mode))
(use-package lsp-java)
(use-package lsp-pyright)
(use-package yasnippet-snippets)
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
	lsp-ui-doc-use-childframe t
	lsp-ui-doc-position 'top
	lsp-ui-doc-include-signature t
	lsp-ui-sideline-enable nil
	lsp-ui-flycheck-enable t
	lsp-ui-flycheck-list-position 'right
	lsp-ui-flycheck-live-reporting t
	lsp-ui-peek-enable t
	lsp-ui-peek-list-width 60
	lsp-ui-peek-peek-height 25))
(use-package company-lsp)

;; treemacs
(use-package treemacs
  :config(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  :bind("C-c s" . treemacs))

;; restart-emacs
(use-package restart-emacs
  :commands(restart-emacs)
  :bind("C-c e r" . restart-emacs))

;; make new windows focused by default
(defadvice split-window (after split-window-after activate)
  (other-window 1))

;; launch dashboard
(use-package page-break-lines)
(use-package all-the-icons)
(use-package dashboard
  :config
  (setq dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory)
	dashboard-items '((agenda . 5)
			  (recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (registers . 5))
	dashboard-show-navigator t
	dashboard-set-init-info t
	dashboard-center-content t
	dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
	)
  (dashboard-setup-startup-hook))

;; dvp input method
(use-package programmer-dvorak
  :hook
  (minibuffer-setup . (lambda () (set-input-method "programmer-dvorak")))
  (char-mode . (lambda () (set-input-method "programmer-dvorak")))
  (line-mode . (lambda () (set-input-method "programmer-dvorak")))
  (emacs-startup . (lambda () (shell-command "im-select com.apple.keylayout.US")))
  (focus-in . (lambda () (shell-command "im-select com.apple.keylayout.US")))
  (focus-out . (lambda () (shell-command "im-select \"com.apple.keyboardlayout.Programmer Dvorak.keylayout.ProgrammerDvorak\"")))
  (kill-emacs . (lambda () (shell-command "im-select \"com.apple.keyboardlayout.Programmer Dvorak.keylayout.ProgrammerDvorak\""))))

;; use fish-completion when fish is installed
(use-package fish-completion
  :when(executable-find "fish")
  :config(global-fish-completion-mode))


;; disable emacs ui elements
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq initial-frame-alist
      '((menu-bar-lines . 0)
	(tool-bar-lines . 0)))
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(blink-cursor-mode 0)
(setq inhibit-startup-echo-area-message "alex")

;; minimalist titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; set the default font
(set-face-attribute 'default nil :font "Mx437 IBM XGA-AI 7x15-15:antialias=false:hinting=true")
(set-face-attribute 'variable-pitch nil :font "Dank Mono-12")
(set-face-attribute 'fixed-pitch nil :inherit 'default)
;; disable bold
(defun disable-bold-on-all-faces ()
  "Sets the weight of all faces to normal"
    (mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil))
  (face-list)))
(add-hook 'after-init-hook 'disable-bold-on-all-faces)

;; automatically revert buffers
(global-auto-revert-mode)

;; scroll line-by-line
(setq scroll-conservatively 101)

;; disable the modeline by default
;; https://bzg.fr/en/emacs-hide-mode-line/
(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
	    mode-line-format nil)
    (setq mode-line-format hide-mode-line
	  hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
	     hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
	     "Use M-x hidden-mode-line-mode to make the mode-line appear."))))
(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)
(hidden-mode-line-mode 1)

(bind-key (kbd "C-c t m") 'hidden-mode-line-mode)

;; vterm terminal emulator
(use-package vterm
  :config
  (add-to-list 'display-buffer-alist
	       '("\*vterm\*"
		 (display-buffer-in-side-window)
		 (window-height . 0.25)
		 (side . bottom)
		 (slot . 0))))
;; popover for vterm
(use-package vterm-toggle
  :after vterm
  :bind
  ("C-," . vterm-toggle))


;; auto balanaces windows
(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618))
  (zoom-ignored-major-modes '(dired-mode markdown-mode vterm-mode))
  (zoom-ignored-buffer-names '("zoom.el" "init.el"))
  (zoom-ignored-buffer-name-regexps '("^*calc"))
  :hook after-init)

;; quick-edit emacs config
(defun edit-config ()
  "Edit the emacs init.el in user-emacs-directory"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(bind-key "C-c d c" 'edit-config)


;; evil binds for org
(use-package evil-org
  :after org
  :after evil
  :after evil-collection
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; evil binds for everything else
(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

;; automatically show keybinds
(use-package which-key
  :disabled t
  :config(which-key-mode))
;; better search for evil
(use-package evil-anzu
  :after evil
  :config
  (with-eval-after-load 'evil
    (require 'evil-anzu)))

;; doesn't need explaining
(use-package evil
  :hook
  (evil-insert-state-entry . (lambda () (set-input-method "programmer-dvorak")))
  (evil-replace-state-entry . (lambda () (set-input-method "programmer-dvorak")))
  (isearch-mode . (lambda () (set-input-method "programmer-dvorak")))
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))


(use-package posframe)

;; quickly switch windows
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
  (ace-window-posframe-mode)
  :bind("C-a" . ace-window))

;; toggleable C-x 1
(use-package zygospore
  :bind("C-x 1" . zygospore-toggle-delete-other-windows))

;; fancy ivy
(use-package ivy-posframe
  :disabled t
  :custom(ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  :config(ivy-posframe-mode))

;; better ingredients, better pizza, better M-x
(use-package counsel
  :config
  (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))
  (counsel-mode 1)
  (ivy-mode 1)
  :bind
  ("M-x" . counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-c p" . counsel-fzf)
  ("C-s" . swiper)
  ("C-x b" . counsel-switch-buffer))

;; company completion for < in org
(use-package company-org-block
  :after company
  :config(add-to-list 'company-backends 'company-org-block))

;; popups
(use-package company
  :bind (:map prog-mode-map
	      ("C-i" . company-indent-or-complete-common)
	      ("C-M-i" . counsel-company))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0)
  :hook prog-mode)

;;  company front-end with icons
(use-package company-box
  :hook company-mode)

;; all themes are safe, it's a security hole, bite me
(setq custom-safe-themes t)
(use-package doom-themes
  :config
  (load-theme 'doom-one-light t)
  (doom-themes-org-config))

;; why this isn't a bind already is beyond me
(bind-key "C-;" 'eval-buffer 'emacs-lisp-mode-map)

;; themes
(global-hl-line-mode +1)
(setq modus-themes-fringes nil
      modus-themes-hl-line 'accented
      modus-themes-subtle-line-numbers t
      modus-themes-org-blocks 'tinted-background
      )

; (bind-key "C-c t d" 'modus-themes-toggle)
; (load-theme 'modus-vivendi t)


;; line numbers are good
(add-hook 'prog-mode-hook 'linum-mode)



(use-package doom-modeline
  :custom
  (doom-modeline-time t)
  :init(display-battery-mode)
  :config(doom-modeline-mode))

;; plantuml for babel
(setq org-plantuml-jar-path (expand-file-name "~/.config/emacs/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;; better pdf viewer
(use-package pdf-tools
  :config
  (setq pdf-view-use-scaling t)
  (pdf-tools-install))



;; hack to scale latex previews
(defun my/text-scale-adjust-latex-previews ()
  "Adjust the size of latex preview fragments when changing the
buffer's text scale."
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
	       'preview-overlay)
	   (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
	       'org-latex-overlay)
	   (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
	 (plist-put
	  (cdr (overlay-get ov 'display))
	  :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))
(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)

;; fast latex input
(use-package cdlatex
  :hook
  (org-mode . turn-on-org-cdlatex))

;; use variable-pitch for text
(use-package mixed-pitch
  :hook text-mode)

;; fancy bullets
(use-package org-bullets
  :hook org-mode)

;; the org that ships normally is old
(use-package org
  :bind
  ("C-c c" . org-capture)
  (:map org-mode-map
	("A-<tab>" . org-show-current-heading-tidily)
	)
  :custom
  (org-preview-latex-image-directory "~/.local/cache/org-previews/")
  (org-default-notes-file "~/Documents/knowledge/main.org")
  (org-capture-templates '(
			   ("n" "Note" entry (file+datetree "~/Documents/knowledge/main.org" "Fleeting notes") "* %?\n%i\n")
			   ))
  :config
  (defun org-show-current-heading-tidily ()
    (interactive)  ;Inteactive
    "Show next entry, keeping other entries closed."
    (if (save-excursion (end-of-line) (outline-invisible-p))
	(progn (org-show-entry) (show-children))
      (outline-back-to-heading)
      (unless (and (bolp) (org-on-heading-p))
	(org-up-heading-safe)
	(hide-subtree)
	(error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)))
  ; (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-agenda-files '("~/Documents/knowledge/main.org"))
  (setq org-src-fontify-natively t)
  (setq org-highlight-latex-and-related '(latex script entities))
  (font-lock-add-keywords 'org-mode
			  '(("\\(\\\\cite\\)" . font-lock-keyword-face)
			    ("\\[[0-9]+]" . font-lock-type-face)
			    ("\\s-*[a-zA-Z]+[0-9]+[a-z]" . font-lock-constant-face)))
  (font-lock-add-keywords 'org-mode
			  '(("\\(\\\\citep\\)" . font-lock-keyword-face)))
  (font-lock-add-keywords 'org-mode
			  '(("\\(\\\\citet\\)" . font-lock-keyword-face)))
  (font-lock-add-keywords 'org-mode
			  '(("\\(\\\\citealp\\)" . font-lock-keyword-face)))
  (font-lock-add-keywords 'org-mode
			  '(("\\(\\\\citeauthor\\)" . font-lock-keyword-face)))
  (font-lock-add-keywords 'org-mode
			  '(("\\(\\\\citeyear\\)" . font-lock-keyword-face)))
  (require 'org-tempo)
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . visual-line-mode))

(use-package names)
(use-package org-latex-impatient
  ; :ensure-system-package (mathjax-node-cli . "npm i -g mathjax-node-cli")
  :hook org-mode
  :config 
  (setq org-latex-impatient-tex2svg-bin "~/.config/emacs/node_modules/.bin/tex2svg"))

(use-package webkit-katex-render
  :disabled t
  :straight (:type git :host github :repo "fuxialexander/emacs-webkit-katex-render")
  :hook org-mode
  :init
  (setq webkit-katex-render--background-color (doom-color 'bg)))

(use-package org-fragtog
  :disabled t
  :hook org-mode)
;; typewriter mode
(use-package centered-cursor-mode
  :commands centered-cursor-mode)
;; zen-mode
(use-package olivetti
  :commands olivetti-mode)

;; search my notes
(bind-key "C-l" #'(lambda () ""
		   (interactive)
		   (find-file "~/Documents/knowledge/main.org")
		   (counsel-org-goto)))

;; interleave notes with pdfs
(use-package org-noter
  :commands(org-noter)
  :config(evil-collection-define-key 'normal 'pdf-view-mode-map
	   (kbd "i") 'org-noter-insert-note))


;; magit
(use-package magit
  :bind("C-c m" . magit)
  :commands(magit))
;; project management
(use-package counsel-projectile
  :hook projectile-mode)
(use-package projectile
  :bind(:map projectile-mode-map
	     ("C-c C-p" . 'projectile-command-map))
  :custom
  (projectile-project-search-path (cddr (directory-files "~/Projects" t)))
  (projectile-completion-system 'ivy))

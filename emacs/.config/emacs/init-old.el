(load-file (expand-file-name "secrets.el" user-emacs-directory))
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
                                        ; (setq native-comp-async-report-warnings-errors nil) (setq straight-disable-native-compile t)

(setq straight-disable-compile t)
(setq straight-host-usernames '((github . "alexeyre")))

;; garbage collection magic hack
(straight-use-package 'gcmh)
(require 'gcmh)
(gcmh-mode 1)

(setq system-time-locale "en_GB")
(setq calendar-date-style 'european)

(setq frame-resize-pixelwise t)

;; no-littering
;; moves emacs data directories and moves backup files
(straight-use-package 'no-littering)
(require 'no-littering)

;; always follow symlinks
(setq vc-follow-symlinks t)

;; use-package and extensions
(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)
(setq use-package-always-ensure nil)
(setq straight-use-package-by-default t)

(use-package variable-pitch
  :straight nil
  :hook org-mode)

(use-package cdlatex
  :hook(org-mode . org-cdlatex-mode))

(defun open-knowledge-file (file-name)
  "Open the file FILE-NAME.org in ~/Documents/knowledge"
  (find-file (expand-file-name file-name "~/Documents/knowledge"))  )
(defun open-knowledge-file/main ()
  "Open main.org in ~/Documents/knowledge"
  (interactive)
  (open-knowledge-file "main.org"))
(bind-key "C-c C-f n" 'open-knowledge-file/main)



(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode))
(use-package laas
  :hook org-mode)
(use-package yasnippet
  :hook (org-mode . yas-minor-mode))

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
;; the org that ships normally is old
(use-package org
  :straight(:type git :repo "https://git.tecosaur.net/tec/org-mode.git")
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  (:map org-mode-map
        ("s-<tab>" . org-show-current-heading-tidily)
        ("s-<return>" . org-insert-heading-after-current)
        )
  :custom
  ;; (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-treat-insert-todo-heading-as-state-change t)
  (org-log-into-drawer t)
  (org-preview-latex-image-directory "~/.local/cache/org-previews/")
  (org-default-notes-file "~/Documents/knowledge/main.org")
                                        ; (org-hide-emphasis-markers t)
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
  (add-to-list 'org-latex-packages-alist '("" "amsmath" t nil))
  (add-to-list 'org-latex-packages-alist '("" "amssymb" t nil))
  (require 'org-agenda)
  (add-to-list 'org-agenda-prefix-format '(todo . "%b"))
                                        ; (plist-put org-format-latex-options :scale 1.5)
  (require 'org-capture)
  (add-to-list 'org-capture-templates '("t" "Todo" entry (file+headline "~/Documents/knowledge/todo.org" "Tasks")
                                        "* TODO %?\n  %i\n  %a"))

  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-agenda-files '("~/Documents/knowledge/main.org" "~/Documents/knowledge/todo.org"))
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
  (org-mode . org-latex-preview-auto-mode)
  (org-mode . org-indent-mode)
  (org-mode . auto-fill-mode)
  (org-mode . visual-line-mode))

(use-package smartparens
  :hook prog-mode org-mode
  :config
  (require 'smartparens-config)
  (sp-with-modes 'org-mode
    (sp-local-pair "$" "$")))

(use-package python
  :straight nil
  :custom
  (python-indent-guess-indent-offset t))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; derive exec-path from $SHELL
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

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
(set-frame-parameter nil 'alpha '(100 . 100))
(bind-key "C-c t t" 'toggle-transparency)
(bind-key "C-c f" 'toggle-frame-fullscreen)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :bind(:map copilot-completion-map
             ("<tab>" . copilot-accept-completion))
  )

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
  :bind
  ("C-c d d" . (lambda () "Goto the dashboard buffer" (interactive) (switch-to-buffer "*dashboard*")))
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory)
        dashboard-agenda-prefix-format "%20b%s"
        dashboard-items '((agenda . 20))
        dashboard-center-content t
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
        )
  (dashboard-setup-startup-hook))

(defun open-current-directory ()
  "Open the current file's directory in Finder"
  (interactive)
  (let ((process-connection-type nil))
    (start-process ""
                   nil
                   "open"
                   (url-file-directory buffer-file-name))))
(bind-key "C-c o" 'open-current-directory)

;; swap meta and alt
;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)

(defun os-layout/qwerty ()
  "Set the OS layout to QWERTY"
  (let ((inhibit-message t)
        (message-log-max nil))
    (call-process-shell-command "im-select com.apple.keylayout.US&" nil 0)))
(defun os-layout/dvp ()
  "Set the OS layout to Programmer Dvorak"
  (let ((inhibit-message t)
        (message-log-max nil))
    (call-process-shell-command "im-select \"com.apple.keyboardlayout.Programmer Dvorak.keylayout.ProgrammerDvorak\"" nil 0)))

;; dvp input method
(use-package programmer-dvorak
  :hook
  (minibuffer-setup . (lambda () (set-input-method "programmer-dvorak")))
  (char-mode . (lambda () (set-input-method "programmer-dvorak")))
  (line-mode . (lambda () (set-input-method "programmer-dvorak")))
  (isearch-mode-end . (lambda () (set-input-method "programmer-dvorak")))
  (emacs-startup . os-layout/qwerty)
  (focus-in . os-layout/qwerty)
  (focus-out . os-layout/dvp)
  (kill-emacs . os-layout/dvp))

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
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-echo-area-message "alex")

;; minimalist titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; ligatures

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; font config
(set-face-attribute 'variable-pitch nil :inherit 'default :family "Overpass" :height 160)
(set-face-attribute 'default nil :family "FiraCode Nerd Font Mono" :height 140)

(use-package mixed-pitch
  :config
  (dolist (face '(org-modern-label
                  org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face))
  :hook org-mode)

;; automatically revert buffers
(global-auto-revert-mode)

;; scroll line-by-line
(setq scroll-conservatively 101)
(setq auto-window-vscroll nil)
(setq scroll-step 1)

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
  (setq vterm-eval-cmds '(("find-file" find-file)
                          ("message" message)
                          ("vterm-clear-scrollback" vterm-clear-scrollback)
                          ("dired" dired)
                          ("ediff-files" ediff-files)))
  (add-to-list 'display-buffer-alist
               '("\*vterm\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0))))
;; popover for vterm
(use-package vterm-toggle
  :bind
  ("C-," . vterm-toggle))

;; auto balances windows
(use-package zoom
  :disabled t
  :custom
  (zoom-size '(0.618 . 0.618))
  (zoom-ignored-major-modes '(dired-mode markdown-mode vterm-mode))
  (zoom-ignored-buffer-names '("zoom.el" "init.el"))
  (zoom-ignored-buffer-name-regexps '("^*calc"))
  :hook (after-init . zoom-mode))
(use-package golden-ratio
  :config
  (setq golden-ratio-auto-scale t)
  (add-to-list 'golden-ratio-exclude-modes 'pdf-view-mode)
  (add-to-list 'golden-ratio-exclude-modes 'calendar-mode)
  (golden-ratio-mode 1))

;; quick-edit emacs config
(defun edit-config ()
  "Edit the emacs init.el in user-emacs-directory"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(bind-key "C-c C-f c" 'edit-config)


;; evil binds for org
(use-package evil-org
  :hook org-mode
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
  :config(which-key-mode))

;; better search for evil
(use-package evil-anzu
  :after evil
  :config
  (require 'evil-anzu))

;; doesn't need explaining
(use-package evil
  :config
  (defun programmer-dvorak-mode ()
    "Set input method to programmer dvorak"
    (set-input-method "programmer-dvorak"))
  :hook
  (evil-motion-state-entry . programmer-dvorak-mode)
  (evil-jump-state-entry . programmer-dvorak-mode)
  (evil-insert-state-entry . programmer-dvorak-mode)
  (evil-replace-state-entry . programmer-dvorak-mode)
  (isearch-mode . programmer-dvorak-mode)
  (evil-insert-state-entry . (lambda () (let ((inhibit-message t)
                                              (message-log-max nil))
                                          (blink-cursor-mode 1))))
  (evil-insert-state-exit . (lambda () (let ((inhibit-message t)
                                             (message-log-max nil))
                                         (blink-cursor-mode -1))))
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :bind("C-\\" . evil-switch-to-windows-last-buffer)
  :config
  (setq evil-vsplit-window-right t
        evil-split-window-below t)
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

;; better ingredients, better pizza, better M-x
(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))

  (defun yf--swiper-advice (&rest r)
    (setq isearch-string (substring-no-properties (car swiper-history)))
    ;;(setq isearch-forward t)
    (evil-search-previous))
  (advice-add 'swiper :after #'yf--swiper-advice)
  :hook((after-init . ivy-mode)
        (ivy-mode . counsel-mode))
  :bind
  ("M-x" . counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-f" . counsel-fzf)
  ("C-s" . swiper)
  ("C-x b" . counsel-switch-buffer))

;; popups
(use-package company
  :bind (:map prog-mode-map
              ("C-i" . company-indent-or-complete-common)
              ("C-M-i" . counsel-company))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0)
  :hook (prog-mode . company-mode))

;;  company front-end with icons
(use-package company-box
  :hook (company-mode . company-box-mode))

;; create a hook for theme changes
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (defun disable-bold-org-headers ()
    "Stop the org-level headers from being bold"
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil :weight 'normal :height 1.0)))
  (add-hook 'org-mode-hook #'disable-bold-org-headers)
  (doom-themes-org-config))

(use-package auto-dark
  :custom
  (auto-dark-allow-osascript t)
  (auto-dark-dark-theme 'doom-xcode)
  (auto-dark-light-theme 'doom-tomorrow-day)
  ;; poll every 5 mins
  (auto-dark-polling-interval-seconds 300)
  :hook
  after-init
  (after-init . auto-dark--check-and-set-dark-mode)
  :config
  (advice-add 'dwim-shell-commands/macos-toggle-dark-mode :after #'(lambda ()
                                                                     "Reassess dark mode on system appearance change"
                                                                     nil
                                                                     (progn
                                                                       (sleep-for 1)
                                                                       (auto-dark--check-and-set-dark-mode)))))

(use-package dwim-shell-command
  :config
  (defun dwim-shell-commands/macos-toggle-dark-mode () "Toggle macOS dark mode."
         (interactive)
         (dwim-shell-command-on-marked-files
          "Toggle dark mode"
          "dark-mode" :utils "dark-mode" ;; brew install dark-mode
          :silent-success t))
  :bind
  ("C-c t l" . 'dwim-shell-commands/macos-toggle-dark-mode))

(use-package solaire-mode
  :config(solaire-global-mode 1))

;; why this isn't a bind already is beyond me
(bind-key "C-;" 'eval-buffer 'emacs-lisp-mode-map)

(use-package hl-line
  :straight nil
  :hook prog-mode)

(use-package doom-modeline
  :custom
  (doom-modeline-time t)
  :init(display-battery-mode)
  :config(doom-modeline-mode))

;; better pdf viewer
(use-package pdf-tools
  :config
  ;; scales pdfs for retina displays
  (setq pdf-view-use-scaling t)
  (pdf-tools-install))

;; fancy bullets
(use-package org-superstar
  :disabled
  :custom
  (org-superstar-special-todo-items t)
  :hook org-mode)

;; prettier org
(use-package org-modern
  :hook org-mode)
(use-package org-modern-indent
  :straight (:type git :host github :repo "jdtsmith/org-modern-indent")
  :hook (org-indent-mode . org-modern-indent-mode)
  :custom-face
  (org-modern-indent-line ((t (:height 1.0 :inherit lem-ui-default-font :inherit lambda-meek)))))

(use-package org-autolist
  :hook org-mode)

(use-package org-appear
  :hook org-mode
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  :config(setq org-hide-emphasis-markers t))

;; automatically hide help windows
(use-package popwin
  :hook after-init)

;; latex hell
(use-package xenops
  :disabled t
  :hook org-mode
  :config
  (org-mode . (lambda () (add-hook 'after-load-theme-hook #'(lambda () (xenops-regenerate)) nil t)))
  (defun xenops-math-reveal (element)
    "Remove image overlay for ELEMENT.
If a prefix argument is in effect, also delete its cache file."
    (xenops-element-overlays-delete element)
    (if current-prefix-arg
        (delete-file (xenops-math-get-cache-file element)))
    ;; TODO: is :begin-content for block math off by one?
    (let ((element-type (plist-get element :type))
          (begin-content (plist-get element :begin-content)))
      )
    (xenops-math-render-below-maybe element))
  (setq xenops-math-latex-process-alist
        '((dvipng :programs
                  ("latex" "dvipng")
                  :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o %f")
                  :image-converter
                  ("dvipng -D %D -T tight -o %O %f"))
          (dvisvgm :programs
                   ("latex" "dvisvgm")
                   :description "xdv > svg"
                   :message "you need to install the programs: latex and dvisvgm."
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler
                   ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -e -n -b min -c %S -o %O"))
          (imagemagick :programs
                       ("latex" "convert")
                       :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                       (1.0 . 1.0)
                       :latex-compiler
                       ("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")
                       :image-converter
                       ("convert -density %D -trim -antialias %f -quality 100 %O"))))
  (setq xenops-font-height 140)
  (setq xenops-font-height-code 140)
  (setq xenops-math-image-scale-factor 1.0)
  (setq xenops-reveal-on-entry t)
  (setq xenops-math-latex-process 'dvisvgm))


(use-package org-latex-impatient
  :disabled
  :hook org-mode
  :config
  (setq org-latex-impatient-tex2svg-bin (executable-find "tex2svg")))

;; typewriter mode
(use-package centered-cursor-mode
  :config
  (setq ccm-recenter-at-end-of-file t)
  :hook (org-mode . centered-cursor-mode)
  :bind(:map text-mode-map
             ("C-c t c" . centered-cursor-mode))
  :commands centered-cursor-mode)

;; zen-mode
(use-package olivetti
  :disabled
  :commands olivetti-mode)

(use-package writeroom-mode
  :bind(:map org-mode-map
             ("C-c t w" . writeroom-mode))
  :commands(writeroom-mode))

;; interleave notes with pdfs
(use-package org-noter
  :commands(org-noter)
  :config
  (evil-collection-define-key 'normal 'pdf-view-mode-map
    (kbd "i") 'org-noter-insert-note)
  (evil-collection-define-key 'normal 'pdf-view-mode-map
    (kbd "I") 'org-noter-insert-note-toggle-no-questions)
  )


;; magit
(use-package magit
  :bind("C-c m" . magit)
  :commands(magit))
;; project management
(use-package counsel-projectile
  :hook (projectile-mode . counsel-projectile-mode)
  :bind
  ("C-c s" . counsel-projectile-switch-project))
(use-package projectile
  :bind(:map projectile-mode-map
             ("C-c C-p" . 'projectile-command-map))
  :custom
  (projectile-project-search-path (cddr (directory-files "~/Projects" t)))
  (projectile-completion-system 'ivy))

(setq warning-suppress-types '((comp)))
(provide 'init)
;;; init.el ends here

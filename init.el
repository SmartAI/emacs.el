;;; Configure the package system
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(require 'use-package)
(setq use-package-always-ensure t)


;;; some basic configuration
(set-fringe-mode 10)        ; Give some breathing room

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; for personal preference
(defvar min/default-font-size 180)
(setq inhibit-startup-message t
      visible-bell t
      ring-bell-function 'ignore)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; disable some fetures
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode t)
(global-display-line-numbers-mode 1)
(column-number-mode 1)

;; osx meta key
(setq mac-command-modifier 'meta)

;; remembering recent edited files
(recentf-mode 1)
;; remembering minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; command log, the functions are clm/xxx
(use-package command-log-mode)

;; theme and font
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t) 
  (load-theme 'doom-palenight t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  )
;; (load-theme 'modus-vivendi-tinted t)

;; font
;; (set-face-attribute 'default nil :font "Fira Code Retina" :height min/default-font-size)
(set-face-attribute 'default nil :font "MesloLGS NF" :height min/default-font-size)

;; Set the fixed pitch face
;; (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 180)
(set-face-attribute 'default nil :font "MesloLGS NF" :height 180)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 220 :weight 'regular)



;; transparent
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; edit
(use-package swiper)

(use-package ivy
  :init
  (ivy-mode 1)
  :after swiper
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  )

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil) ; Don't start serches with ^
  )

(use-package  doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :demand t
  :after evil
  :diminish which-key-mode
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-remaining-keys t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  
  (which-key-mode 1)
  (which-key-setup-minibuffer)
  (set-face-attribute
   'which-key-local-map-description-face nil :weight 'bold))


;; helpful
(use-package helpful
  :custom
  (counsel-describe-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )


;; icons
;; need to execute all-the-icons-install-fonts
;; command
(use-package all-the-icons)
;; key for switch buffer 
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)


;; general.el
;; define key more easily
(use-package general)
;; define my own prefix function
;; and this function can be used
;; to define the key bindings
(general-create-definer min/leader-keys
  :prefix "SPC"
  )

;; evil package settings
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-want-C-i-jump nil
	evil-want-Y-yank-to-eol t
	evil-split-window-below t
	evil-vsplit-window-right t
	evil-respect-visual-line-mode t
	evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  ;; in the insert mode
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; move when the lines are wraped
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; collections
;; give you a stable evil configuration for
;; different modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;; hydra setting
;; tie related commands into a family of short bindings
;; with a common prefix
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))



;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Workspace")
    (setq projectile-project-search-path '("~/Workspace")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package envrc
  :hook (after-init . envrc-global-mode))



(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(javascript typescript tsx css html))
  (treesit-auto-add-to-auto-mode-alist '(javascript typescript tsx css html))
  (global-treesit-auto-mode))



(defun min/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))


;;; LSP settings
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . min/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)


;; Typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))


;; check
(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
	      ("M-n" . flycheck-previous-error)
	      ("M-p" . flycheck-next-error))
  :custom (flycheck-display-errors-delay .3))


;; completion something like company
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
	      ("TAB"        . corfu-next)
	      ([tab]        . corfu-next)
	      ("S-TAB"      . corfu-previous)
	      ([backtab]    . corfu-previous)
	      ("S-<return>" . corfu-insert)
	      ("RET"        . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; Adds icons to the pop-up window
(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; formatting using prettier
(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'prettier apheleia-formatters)
	'("prettier" "--stdin-filepath" filepath)))


;;; for c/c++ 



;;; terminal
(use-package term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))


(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))




;;; Org mode settings  ---------------------------
(defun min/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(defun min/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(use-package org
  :hook (org-mode . min/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; agenda files
  (setq org-agenda-files
	'("~/Workspace/orgfiles/tasks.org"
	  "~/Workspace/orgfiles/habits.org"
	  "~/Workspace/orgfiles/birthdays.org"
	  ))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)
	  ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))


  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work-email")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))

	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))


  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/Workspace/orgfiles/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
           (file+olp+datetree "~/Workspace/orgfiles/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
	  ("jm" "Meeting" entry
           (file+olp+datetree "~/Workspace/orgifles/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

	  ("w" "Workflows")
	  ("we" "Checking Email" entry (file+olp+datetree "~/Workspace/orgfiles/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

	  ("m" "Metrics Capture")
	  ("mw" "Weight" table-line (file+headline "~/Workspace/orgfiles/metrics.org" "Weight")
	   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
	  ("mh" "Hours" table-line (file+headline "~/Workspace/orgfiles/metrics.org" "Workd Hours")
	   "| %U | %^{Work Hours} | %^{Notes} |" :kill-buffer t)
	  ))

  
  (define-key global-map (kbd "C-c j")
	      (lambda () (interactive) (org-capture nil "jj")))

  
  (min/org-font-setup))


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(defun min/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . min/org-mode-visual-fill))


;; undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;;; Dired

;; issue of dired in macos
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/383
(setq insert-directory-program "gls" dired-use-ls-dired t)

(setq dired-listing-switches "-al --group-directories-first")


(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))


(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))


;;; key bindings
(min/leader-keys
  ;; at the normal mode
  :keymaps 'normal
  ;; toggles
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "ts" '(hydra-text-scale/body :which-key "scale text")
  ;; save buffer
  "w" '(save-buffer :which-key "save buffer")

  ;; org setting
  "o" '(:ignore t :which-key "org")
  "oa"'(org-agenda :which-key "org agenda")
  "oc"'(org-capture :which-key "org capture")


  ;; open files
  "f" '(:ignore t :which-key "files")
  "ff" '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "recent file")
  )




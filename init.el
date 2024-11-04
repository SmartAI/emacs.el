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
(setq inhibit-startup-message t
      visible-bell t
      ring-bell-function 'ignore
      )

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; disable some fetures
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode t)
(global-display-line-numbers-mode 1)
(column-number-mode 1)

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
(set-face-attribute 'default nil :font "Fira Code Retina" :height 180)

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

(min/leader-keys
  ;; at the normal mode
  :keymaps 'normal
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme"))

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

;; hydra
;; tie related commands into a family of short bindings
;; with a common prefix
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; resize font
(min/leader-keys
  :keymaps 'normal
  "ts" '(hydra-text-scale/body :which-key "scale text"))


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

(use-package evil-magit
  :after magit)


(use-package envrc
  :hook (after-init . envrc-global-mode))



(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(javascript typescript tsx css html))
  (treesit-auto-add-to-auto-mode-alist '(javascript typescript tsx css html))
  (global-treesit-auto-mode))


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (read-process-output-max (* 1024 1024))
  :init
  (setq lsp-completion-provider :none)
  (setq lsp-keymap-prefix "C-c")
  (setq lsp-diagnostics-provider :flycheck))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))


(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
	      ("M-n" . flycheck-previous-error)
	      ("M-p" . flycheck-next-error))
  :custom (flycheck-display-errors-delay .3))


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


(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'prettier apheleia-formatters)
	'("prettier" "--stdin-filepath" filepath)))




;;; for c/c++ 





;;; 

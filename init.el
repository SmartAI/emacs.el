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
(load-theme 'modus-vivendi-tinted t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 180)

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


(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-want-Y-yank-to-eol t
	evil-split-window-below t
	evil-vsplit-window-right t
	evil-respect-visual-line-mode t
	evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))



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





;;; for version control



;;; 

;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install packages in this list with M-x package-install-selected-packages
;; We only need to list the packages that are not bundled with Emacs
(setq package-selected-packages
      '(olivetti
	      denote
	      iedit
	      vertico
	      marginalia
	      flycheck
	      spell-fu
        haskell-ts-mode
        lsp-mode
        geiser-chez))

;; All my custom code is inside the custom directory
(add-to-list 'load-path "~/.emacs.d/custom")

;; Color palette
(load "colors")

;; Custom functions
(load "functions")

(use-package dired
  :custom
  (dired-listing-switches "-alh")
  :config
  (set-face-foreground 'dired-directory flexoki-blue-700))

;; A better experience for writing text
(use-package olivetti
  :hook org-mode)

;; Automatically create better file names for my Notebook
(use-package denote
  :defer t
  :config
  (setq denote-directory (expand-file-name "~/Developer/Notebook")))

;; Display the undo tree
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; Simultaneous editing of occurrences
;; TODO: Update the color to a variable from colors.el
(use-package iedit
  :ensure t
  :defer t
  :config
  (set-face-background 'iedit-occurrence "#E1ECEB"))

;; Show what functions are available on M-x
(use-package vertico
  :ensure t
  :defer t
  :init
  (vertico-mode))

;; Display descriptions of functions
(use-package marginalia
  :ensure t
  :defer t
  :init
  (marginalia-mode))

;; Persist history over Emacs restarts.
;; - Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Enable finding functions, variables, etc. Without being precise in the spelling.
(use-package orderless
  :defer t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))

;; .editorconfig file support
(use-package editorconfig
  :ensure t
  :defer t
  :delight
  :config
  (editorconfig-mode +1))

;; Enable code checking
(use-package flycheck
  :ensure t
  :custom
  (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  :init
  (global-flycheck-mode))

;; Spell check for natural language
;; Requires Aspell
(use-package spell-fu
  :ensure t
  :config
  (spell-fu-global-mode)
  :custom-face
  (spell-fu-incorrect-face ((t (:underline nil :style wave :color ,flexoki-yellow-200))))
  :custom
  (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc dired-mode)))

(load "languages")

;; =============================================================================
;; General settings

;; How total number of matches when searching
(setq isearch-lazy-count t)

;; Delay inline suggestions by 2 seconds
(setq completion-preview-idle-delay 2)

;; ;; Never use tabs for indentation
(setq-default indent-tabs-mode nil)

;; Always use 2 spaces for indentation
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default c-basic-offset 2)

;; Show recent files when invoking find-file
(recentf-mode 1)

;; Enable ANSI colors when using compile mode
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Auto-scroll when something repeatedly in the compile mode
(with-eval-after-load 'compile
  (setq compilation-scroll-output t))

;; Show what keybindings are available after a prefix like C-x or C-c.
(setq which-key-separator " → "
      which-key-max-display-columns 1 ; We need a single column to have space for the symbol description
      which-key-popup-type 'side-window
      which-key-side-window-location 'bottom
      which-key-side-window-max-width 0.75
      which-key-max-description-length 0.9
      which-key-show-docstrings t)
(which-key-mode t)

;; Keep buffers in sync with file changes
(global-auto-revert-mode t)

;; Keep buffers in sync with directory changes
(setq global-auto-revert-non-file-buffers t)

;; Don't show a startup screen
(setq inhibit-startup-screen t)

;; Avoid having backup and autosave files everywhere
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))

;; Make sure there is always a new line at the end of the file
(setq require-final-newline t)

;; Remove trailing whitespace when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Remove the dash separator in the mode line
(setq-default mode-line-end-spaces "")

;; Hide menu bar
(menu-bar-mode -1)

;; Ask for Y or N instead of Yes or No
(defalias 'yes-or-no-p 'y-or-n-p)

;; Display line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Display column numbers
(setq column-number-mode t)

;; Enable auto-completion for code and text
(global-completion-preview-mode)

;; Match delimiters
(setq electric-pair-preserve-balance t)
(setq electric-pair-delete-adjacent-pairs t)
(electric-pair-mode)

;; ==================================================
;; Keybindings

;; Avoid the the mistake of calling "C-x C-b" instead of "C-x b"
(global-unset-key (kbd "C-x C-b"))

;; Redefine core keybindings
(define-key global-map (kbd "C-a") '("Go to beginning of line" . custom/beginning-of-line))

;; Clipboard keybindings
(define-key global-map (kbd "C-c c") '("Copy to clipboard" . custom/copy-to-clipboard))
(define-key global-map (kbd "C-c v") '("Paste from clipboard" . custom/paste-from-clipboard))

(define-key global-map (kbd "C-c d") '("Duplicate line" . custom/duplicate-line))
(define-key global-map (kbd "C-x C-r") '("Open recent file" . recentf-open))

;; ================================================================================
;; Color customization

;; ANSI Colors
(set-face-foreground 'ansi-color-black flexoki-black)
(set-face-background 'ansi-color-black flexoki-black)
(set-face-foreground 'ansi-color-bright-black flexoki-base-600)
(set-face-background 'ansi-color-bright-black flexoki-base-600)
(set-face-foreground 'ansi-color-blue flexoki-blue-600)
(set-face-background 'ansi-color-blue flexoki-blue-600)
(set-face-foreground 'ansi-color-bright-blue flexoki-blue-400)
(set-face-background 'ansi-color-bright-blue flexoki-blue-400)
(set-face-foreground 'ansi-color-cyan flexoki-cyan-600)
(set-face-background 'ansi-color-cyan flexoki-cyan-600)
(set-face-foreground 'ansi-color-bright-cyan flexoki-cyan-400)
(set-face-background 'ansi-color-bright-cyan flexoki-cyan-400)
(set-face-foreground 'ansi-color-magenta flexoki-magenta-600)
(set-face-background 'ansi-color-magenta flexoki-magenta-600)
(set-face-foreground 'ansi-color-bright-magenta flexoki-magenta-400)
(set-face-background 'ansi-color-bright-magenta flexoki-magenta-400)
(set-face-foreground 'ansi-color-red flexoki-red-600)
(set-face-background 'ansi-color-red flexoki-red-600)
(set-face-foreground 'ansi-color-bright-red flexoki-red-400)
(set-face-background 'ansi-color-bright-red flexoki-red-400)
(set-face-foreground 'ansi-color-green flexoki-green-600)
(set-face-background 'ansi-color-green flexoki-green-600)
(set-face-foreground 'ansi-color-bright-green flexoki-green-400)
(set-face-background 'ansi-color-bright-green flexoki-green-400)
(set-face-foreground 'ansi-color-yellow flexoki-yellow-300)
(set-face-background 'ansi-color-yellow flexoki-yellow-300)
(set-face-foreground 'ansi-color-bright-yellow flexoki-yellow-150)
(set-face-background 'ansi-color-bright-yellow flexoki-yellow-150)

;; Global UI
(set-face-background 'mode-line flexoki-base-50)
(set-face-foreground 'mode-line flexoki-base-800)
(set-face-background 'mode-line-inactive flexoki-base-50)
(set-face-foreground 'mode-line-inactive flexoki-base-400)
(set-face-background 'show-paren-match nil)
(set-face-foreground 'show-paren-match flexoki-red-300)
(set-face-foreground 'line-number flexoki-base-200)
(set-face-foreground 'line-number-current-line flexoki-red-300)
(set-face-background 'region flexoki-yellow-50)

;; Syntax highlighting
(set-face-attribute 'font-lock-function-name-face nil :foreground flexoki-black)
(set-face-attribute 'font-lock-function-call-face nil :foreground flexoki-black)
(set-face-attribute 'font-lock-variable-name-face nil :foreground flexoki-black)
(set-face-attribute 'font-lock-variable-use-face nil :foreground flexoki-black)
(set-face-attribute 'font-lock-keyword-face nil :foreground flexoki-black)
(set-face-attribute 'font-lock-comment-face nil :foreground flexoki-base-500 :slant 'italic)
(set-face-attribute 'font-lock-type-face nil :foreground flexoki-black)
(set-face-attribute 'font-lock-constant-face nil :foreground flexoki-black)
(set-face-attribute 'font-lock-builtin-face nil :foreground flexoki-black)
(set-face-attribute 'font-lock-string-face nil :foreground flexoki-base-600 :slant 'italic)
(set-face-attribute 'font-lock-number-face nil :foreground flexoki-base-600)
(set-face-attribute 'font-lock-operator-face nil :foreground flexoki-base-600 :weight 'light :slant 'normal)
(set-face-attribute 'font-lock-punctuation-face nil :foreground flexoki-base-500)
(set-face-attribute 'font-lock-bracket-face nil :foreground flexoki-base-500)
(set-face-attribute 'font-lock-delimiter-face nil :foreground flexoki-base-500)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((haskell-ts-mode :url "git@github.com:Jaiheravi/haskell-ts-mode.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

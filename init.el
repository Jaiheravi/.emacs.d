;;; -*- lexical-binding: t -*-
;;; -*- no-byte-compile: t -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install packages in this list with M-x package-install-selected-packages
;; We only need to list the packages that are not bundled with Emacs
;; I do this manually instead of relying on the automatic method of Emacs because
;; I like to know what's being installed and make sure it's all intentional.
(setq package-selected-packages
      '(olivetti
        denote
        iedit
        vertico
        marginalia
        flycheck
        spell-fu
        lsp-mode
        geiser-chez
        yasnippet
        expand-region
        multiple-cursors
        visual-fill-column
        swift-mode
        lsp-sourcekit
        swift-helpful
        paren-face
        treemacs))

;; Install packages in this list with M-x package-vc-install-selected-packages
(setq package-vc-selected-packages
   '((haskell-ts-mode :url "git@github.com:Jaiheravi/haskell-ts-mode.git")))

;; All my custom code is inside the custom directory
(add-to-list 'load-path "~/.emacs.d/custom")

;; Global Variables
(load "variables")

;; Color palette
(load "colors")

;; Custom functions
(load "functions")

(load "keybindings")

(use-package hideshow
  :config
  (keymap-global-set (getkey "hs-toggle-hiding") 'hs-toggle-hiding))

(use-package treemacs
  :ensure t
  :config
  (keymap-global-set (getkey "treemacs") 'treemacs)
  (set-face-attribute 'treemacs-git-modified-face nil :foreground rose-pine :slant 'normal)
  (set-face-attribute 'treemacs-git-added-face nil :foreground rose-pine))

;; Add parentheses face everywhere
(use-package paren-face
  :ensure t
  :hook (prog-mode . paren-face-mode)
  :init
  (global-paren-face-mode)
  :config
  (set-face-attribute 'parenthesis nil :foreground rose-highlight-high :weight 'bold)
  (setq paren-face-modes '(prog-mode))
  (setq paren-face-regexp "[][()}{]"))

;; TODO: Check out Combobulate

;; --------------------------------------------------
;; LaTeX

;; Center text
(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-center-text t)
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-for-vline))

(use-package auctex
  :ensure t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (fill-column 80)
  :config
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'AUCTeX-mode t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (auto-fill-mode -1)
              (visual-line-mode)
              (whitespace-mode)
              (set-face-attribute 'whitespace-space nil :background nil :foreground rose-gold :weight 'bold)
              ;; We don't want highlighting long lines on LaTeX documents
              ;; because every paragraph is a line.
              (set-face-attribute 'whitespace-line nil :background nil :foreground nil)
              ;; I use as little syntax highlighting as possible, but LaTeX is unusable without it
              ;; Here I fix highlighting only for LaTeX modes.
              (face-remap-add-relative 'font-lock-keyword-face :foreground rose-gold)
              (font-lock-update))))

;; Select and edit multiple things at the same time
(use-package multiple-cursors
  :ensure t
  :config
  (keymap-global-set (getkey "mc/mark-next-like-this") 'mc/mark-next-like-this)
  (keymap-global-set (getkey "mc/mark-previous-like-this") 'mc/mark-previous-like-this)
  (keymap-global-set (getkey "mc/mark-all-like-this") 'mc/mark-all-like-this))

;; Version Control
(use-package magit
  :config
  (set-face-attribute
   'magit-diff-added nil :background rose-surface :foreground rose-foam)
  (set-face-attribute
   'magit-diff-removed nil :background rose-surface :foreground rose-rose)
  (set-face-attribute
   'magit-diff-added-highlight nil :background rose-base :foreground rose-foam)
  (set-face-attribute
   'magit-diff-removed-highlight nil
   :background rose-base :foreground rose-rose)
  (set-face-attribute
   'magit-diff-context-highlight nil
   :background rose-surface :foreground rose-muted)
  (set-face-attribute
   'magit-diff-hunk-heading-highlight nil
   :background rose-overlay :foreground rose-muted :weight 'bold)
  (set-face-attribute
   'magit-diff-hunk-heading nil
   :background rose-overlay :foreground rose-muted :weight 'normal)
  (set-face-attribute
   'magit-section-heading nil :background nil :foreground rose-foam))

;; Increase selection in a semantic way
(use-package expand-region
  :ensure t
  :config
  (keymap-global-set (getkey "er/expand-region") 'er/expand-region))

(use-package dired
  :custom
  (dired-listing-switches "-alh")
  (dired-dwim-target t)
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
  :config
  (keymap-global-set (getkey "vundo") 'vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

;; Simultaneous editing of occurrences
;; To avoid editing all instances, it's a good idea to combine with:
;; C-x n n -> narrow-to-region
;; C-x n w -> widen (restore full buffer view)
(put 'narrow-to-region 'disabled nil) ; Enable narrow-to-region
(use-package iedit
  :ensure t
  :defer t
  :config
  (keymap-global-set (getkey "iedit-mode") 'iedit-mode)
  (set-face-background 'iedit-occurrence rose-overlay))

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

;; Make all whitespace characters visible
(use-package whitespace
  :config
  (set-face-attribute 'whitespace-line nil
                      :background rose-overlay
                      :foreground rose-rose))

;; Persist history over Emacs restarts.
;; - Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Enable finding functions, variables, etc.
;; Without being precise in the spelling.
(use-package orderless
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
  (spell-fu-incorrect-face
   ((t (:underline nil :style wave :color ,flexoki-yellow-200))))
  :custom
  (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc dired-mode)))

;; Quickly insert bits of code
(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :init
  (yas-global-mode 1))

(load "languages")

;; --------------------------------------------------
;; General settings

;; Automatically wrap comments
(setq comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; Dictionary
(setq dictionary-server "dict.org")

;; How total number of matches when searching
(setq isearch-lazy-count t)

;; Delay inline suggestions by 2 seconds
(setq completion-preview-idle-delay 1)

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
(use-package which-key
  :config
  (setq which-key-separator " → "
        which-key-max-display-columns 1
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-width 0.75
        which-key-max-description-length 0.9
        which-key-show-docstrings t)
  :init
  (which-key-mode t))

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

;; --------------------------------------------------
;; Color customization

;; ANSI Colors
(set-face-foreground 'ansi-color-black rose-text)
(set-face-background 'ansi-color-black rose-text)
(set-face-foreground 'ansi-color-bright-black rose-muted)
(set-face-background 'ansi-color-bright-black rose-muted)
(set-face-foreground 'ansi-color-blue rose-pine)
(set-face-background 'ansi-color-blue rose-pine)
(set-face-foreground 'ansi-color-bright-blue rose-pine)
(set-face-background 'ansi-color-bright-blue rose-pine)
(set-face-foreground 'ansi-color-cyan rose-iris)
(set-face-background 'ansi-color-cyan rose-iris)
(set-face-foreground 'ansi-color-bright-cyan rose-iris)
(set-face-background 'ansi-color-bright-cyan rose-iris)
(set-face-foreground 'ansi-color-magenta rose-iris)
(set-face-background 'ansi-color-magenta rose-iris)
(set-face-foreground 'ansi-color-bright-magenta rose-iris)
(set-face-background 'ansi-color-bright-magenta rose-iris)
(set-face-foreground 'ansi-color-red rose-love)
(set-face-background 'ansi-color-red rose-love)
(set-face-foreground 'ansi-color-bright-red rose-love)
(set-face-background 'ansi-color-bright-red rose-love)
(set-face-foreground 'ansi-color-green rose-foam)
(set-face-background 'ansi-color-green rose-foam)
(set-face-foreground 'ansi-color-bright-green rose-foam)
(set-face-background 'ansi-color-bright-green rose-foam)
(set-face-foreground 'ansi-color-yellow rose-gold)
(set-face-background 'ansi-color-yellow rose-gold)
(set-face-foreground 'ansi-color-bright-yellow rose-gold)
(set-face-background 'ansi-color-bright-yellow rose-gold)

;; Global UI
(set-face-attribute 'mode-line nil
                    :background rose-highlight-low
                    :foreground rose-subtle
                    :box '(:style flat-button :line-width 4))

(set-face-attribute 'mode-line-inactive nil
                    :background rose-surface
                    :foreground rose-muted
                    :box '(:style flat-button :line-width 4))

(set-face-attribute 'default nil
                    :foreground rose-text
                    :background rose-surface)

(set-face-background 'show-paren-match nil)
(set-face-foreground 'show-paren-match rose-rose)
(set-face-foreground 'line-number rose-highlight-high)
(set-face-foreground 'line-number-current-line rose-rose)
(set-face-background 'region rose-highlight-low)
(set-face-attribute 'isearch nil :background rose-overlay :foreground rose-love)
(set-face-attribute 'lazy-highlight nil
                    :background rose-overlay :foreground rose-love)
(set-face-attribute 'minibuffer-prompt nil
                    :foreground rose-rose :weight 'normal)
(set-face-attribute 'highlight nil :background rose-overlay)

;; Syntax highlighting
(set-face-attribute 'font-lock-function-name-face nil :foreground rose-text)
(set-face-attribute 'font-lock-function-call-face nil :foreground rose-text)
(set-face-attribute 'font-lock-variable-name-face nil :foreground rose-text :slant 'italic)
(set-face-attribute 'font-lock-variable-use-face nil :foreground rose-text)
(set-face-attribute 'font-lock-keyword-face nil :foreground rose-text :weight 'bold)
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic
                    :foreground rose-rose)
(set-face-attribute 'font-lock-type-face nil :foreground rose-text)
(set-face-attribute 'font-lock-constant-face nil :foreground rose-text)
(set-face-attribute 'font-lock-builtin-face nil :foreground rose-pine)
(set-face-attribute 'font-lock-string-face nil
                    :slant 'italic
                    :foreground rose-subtle)
(set-face-attribute 'font-lock-number-face nil :foreground rose-subtle)
(set-face-attribute 'font-lock-operator-face nil :foreground rose-subtle)
(set-face-attribute 'font-lock-punctuation-face nil :foreground rose-muted)
(set-face-attribute 'font-lock-bracket-face nil :foreground rose-muted)
(set-face-attribute 'font-lock-delimiter-face nil :foreground rose-muted)
(set-face-attribute 'font-lock-escape-face nil :foreground rose-rose)

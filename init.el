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
        lsp-mode
        geiser-chez
        latex-preview-pane))

;; Install packages in this list with M-x package-vc-install-selected-packages
(setq package-vc-selected-packages
   '((haskell-ts-mode :url "git@github.com:Jaiheravi/haskell-ts-mode.git")))

;; All my custom code is inside the custom directory
(add-to-list 'load-path "~/.emacs.d/custom")

;; Color palette
(load "colors")

;; Custom functions
(load "functions")

(use-package magit
  :config
  (set-face-attribute 'magit-diff-added nil :background rose-surface :foreground rose-foam)
  (set-face-attribute 'magit-diff-removed nil :background rose-surface :foreground rose-rose)
  (set-face-attribute 'magit-diff-added-highlight nil :background rose-base :foreground rose-foam)
  (set-face-attribute 'magit-diff-removed-highlight nil :background rose-base :foreground rose-rose)
  (set-face-attribute 'magit-diff-context-highlight nil :background rose-surface :foreground rose-muted)
  (set-face-attribute 'magit-diff-hunk-heading-highlight nil :background rose-overlay :foreground rose-muted :weight 'bold)
  (set-face-attribute 'magit-diff-hunk-heading nil :background rose-overlay :foreground rose-muted :weight 'normal)
  (set-face-attribute 'magit-section-heading nil :background nil :foreground rose-foam))

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
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

;; Simultaneous editing of occurrences
(use-package iedit
  :ensure t
  :defer t
  :bind
  ("C-c i" . iedit-mode)
  :config
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

;; --------------------------------------------------
;; GUI Settings
;;
;; I use the Emacs GUI when I write LaTex documents,
;; so I need to clean things up for that. Everything
;; else assumes use in the terminal.

(if (display-graphic-p)
    (progn
      ;; No tool bar
      (tool-bar-mode -1)

      ;; No scroll bar
      (scroll-bar-mode -1)

      ;; Meta key
      (setq mac-option-modifier 'meta)
      (setq mac-right-option-modifier nil)

      ;; Blend the fringe with the background
      (set-face-attribute 'fringe nil
                          :background rose-surface
                          :foreground rose-overlay)
      ;; Use "paper" color for the frames
      ;; TODO: I should use a variable here but I didn't know how
      (add-to-list 'default-frame-alist '(background-color . "#fffaf3"))
      (set-face-attribute 'default nil
                          :family "JetBrains Mono"
                          :height 160)))

;; ==================================================
;; Keybindings

;; Avoid the the mistake of calling "C-x C-b" instead of "C-x b"
(global-unset-key (kbd "C-x C-b"))

;; Redefine core keybindings
(define-key global-map (kbd "C-a") '("Go to beginning of line" . custom/beginning-of-line))

;; Clipboard keybindings
(define-key global-map (kbd "C-c c") '("Copy to clipboard" . custom/copy-to-clipboard))
(define-key global-map (kbd "C-c v") '("Paste from clipboard" . custom/paste-from-clipboard))

;; Other
(define-key global-map (kbd "C-c d") '("Duplicate line" . custom/duplicate-line))
(define-key global-map (kbd "C-x C-r") '("Open recent file" . recentf-open))

;; ================================================================================
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
(set-face-attribute 'lazy-highlight nil :background rose-base :foreground rose-love)
(set-face-attribute 'minibuffer-prompt nil :foreground rose-rose :weight 'normal)
(set-face-attribute 'highlight nil :background rose-overlay)

;; Syntax highlighting
(set-face-attribute 'font-lock-function-name-face nil :foreground rose-text)
(set-face-attribute 'font-lock-function-call-face nil :foreground rose-text)
(set-face-attribute 'font-lock-variable-name-face nil :foreground rose-text)
(set-face-attribute 'font-lock-variable-use-face nil :foreground rose-text)
(set-face-attribute 'font-lock-keyword-face nil :foreground rose-text)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic :foreground rose-rose)
(set-face-attribute 'font-lock-type-face nil :foreground rose-text)
(set-face-attribute 'font-lock-constant-face nil :foreground rose-text)
(set-face-attribute 'font-lock-builtin-face nil :foreground rose-text)
(set-face-attribute 'font-lock-string-face nil :slant 'italic :foreground rose-subtle)
(set-face-attribute 'font-lock-number-face nil :foreground rose-subtle)
(set-face-attribute 'font-lock-operator-face nil :foreground rose-subtle)
(set-face-attribute 'font-lock-punctuation-face nil :foreground rose-subtle)
(set-face-attribute 'font-lock-bracket-face nil :foreground rose-muted)
(set-face-attribute 'font-lock-delimiter-face nil :foreground rose-muted)

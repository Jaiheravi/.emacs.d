;; Prevent package.el loading packages prior to their init-file loading
;;   This is a recommendation from the documentation of straight.el
(setq package-enable-at-startup nil)

;; ================================================================================
;; Colors
;; Palette from https://github.com/kepano/flexoki

;; Base
(setq flexoki-base-paper "#FFFCF0"
      flexoki-base-50 "#F2F0E5"
      flexoki-base-100 "#E6E4D9"
      flexoki-base-150 "#DAD8CE"
      flexoki-base-200 "#CECDC3"
      flexoki-base-300 "#B7B5A9"
      flexoki-base-400 "#9F9D96"
      flexoki-base-500 "#878580"
      flexoki-base-600 "#6F6E69"
      flexoki-base-700 "#575653"
      flexoki-base-800 "#403E3C"
      flexoki-base-850 "#343331"
      flexoki-base-900 "#282726"
      flexoki-base-950 "#1C1B1A"
      flexoki-black "#100F0F")

;; Red
(setq flexoki-red-50 "#FFE1D5"
      flexoki-red-100 "#FFCABB"
      flexoki-red-150 "#FDB2A2"
      flexoki-red-200 "#F89A8A"
      flexoki-red-300 "#E8705F"
      flexoki-red-400 "#D14D41"
      flexoki-red-500 "#C03E35"
      flexoki-red-600 "#AF3029"
      flexoki-red-700 "#942822"
      flexoki-red-800 "#6C201C"
      flexoki-red-850 "#551B18"
      flexoki-red-900 "#3E1715"
      flexoki-red-950 "#261312")

;; Orange
(setq flexoki-orange-50 "#FFE7CE"
      flexoki-orange-100 "#FED3AF"
      flexoki-orange-150 "#FCC192"
      flexoki-orange-200 "#F9AE77"
      flexoki-orange-300 "#EC8B49"
      flexoki-orange-400 "#DA702C"
      flexoki-orange-500 "#CB6120"
      flexoki-orange-600 "#BC5215"
      flexoki-orange-700 "#9D4310"
      flexoki-orange-800 "#71320D"
      flexoki-orange-850 "#59290D"
      flexoki-orange-900 "#40200D"
      flexoki-orange-950 "#27180E")

;; Yellow
(setq flexoki-yellow-50 "#FAEEC6"
      flexoki-yellow-100 "#F6E2A0"
      flexoki-yellow-150 "#F1D67E"
      flexoki-yellow-200 "#ECCB60"
      flexoki-yellow-300 "#DFB431"
      flexoki-yellow-400 "#D0A215"
      flexoki-yellow-500 "#BE9207"
      flexoki-yellow-600 "#AD8301"
      flexoki-yellow-700 "#8E6B01"
      flexoki-yellow-800 "#664D01"
      flexoki-yellow-850 "#503D02"
      flexoki-yellow-900 "#3A2D04"
      flexoki-yellow-950 "#241E08")

; Green
(setq flexoki-green-50 "#EDEECF"
      flexoki-green-100 "#DDE2B2"
      flexoki-green-150 "#CDD597"
      flexoki-green-200 "#BEC97E"
      flexoki-green-300 "#A0AF54"
      flexoki-green-400 "#879A39"
      flexoki-green-500 "#768D21"
      flexoki-green-600 "#66800B"
      flexoki-green-700 "#536907"
      flexoki-green-800 "#3D4C07"
      flexoki-green-850 "#313D07"
      flexoki-green-900 "#252D09"
      flexoki-green-950 "#1A1E0C")

;; Cyan
(setq flexoki-cyan-50 "#DDF1E4"
      flexoki-cyan-100 "#BFE8D9"
      flexoki-cyan-150 "#A2DECE"
      flexoki-cyan-200 "#87D3C3"
      flexoki-cyan-300 "#5ABDAC"
      flexoki-cyan-400 "#3AA99F"
      flexoki-cyan-500 "#2F968D"
      flexoki-cyan-600 "#24837B"
      flexoki-cyan-700 "#1C6C66"
      flexoki-cyan-800 "#164F4A"
      flexoki-cyan-850 "#143F3C"
      flexoki-cyan-900 "#122F2C"
      flexoki-cyan-950 "#101F1D")

;; Blue
(setq flexoki-blue-50 "#E1ECEB"
      flexoki-blue-100 "#C6DDE8"
      flexoki-blue-150 "#ABCFE2"
      flexoki-blue-200 "#92BFDB"
      flexoki-blue-300 "#66A0C8"
      flexoki-blue-400 "#4385BE"
      flexoki-blue-500 "#3171B2"
      flexoki-blue-600 "#205EA6"
      flexoki-blue-700 "#1A4F8C"
      flexoki-blue-800 "#163B66"
      flexoki-blue-850 "#133051"
      flexoki-blue-900 "#12253B"
      flexoki-blue-950 "#101A24")

;; Purple
(setq flexoki-purple-50 "#F0EAEC"
      flexoki-purple-100 "#E2D9E9"
      flexoki-purple-150 "#D3CAE6"
      flexoki-purple-200 "#C4B9E0"
      flexoki-purple-300 "#A699D0"
      flexoki-purple-400 "#8B7EC8"
      flexoki-purple-500 "#735EB5"
      flexoki-purple-600 "#5E409D"
      flexoki-purple-700 "#4F3685"
      flexoki-purple-800 "#3C2A62"
      flexoki-purple-850 "#31234E"
      flexoki-purple-900 "#261C39"
      flexoki-purple-950 "#1A1623")

;; Magenta
(setq flexoki-magenta-50 "#FEE4E5"
      flexoki-magenta-100 "#FCCFDA"
      flexoki-magenta-150 "#F9B9CF"
      flexoki-magenta-200 "#F4A4C2"
      flexoki-magenta-300 "#E47DA8"
      flexoki-magenta-400 "#CE5D97"
      flexoki-magenta-500 "#B74583"
      flexoki-magenta-600 "#A02F6F"
      flexoki-magenta-700 "#87285E"
      flexoki-magenta-800 "#641F46"
      flexoki-magenta-850 "#4F1B39"
      flexoki-magenta-900 "#39172B"
      flexoki-magenta-950 "#24131D")


;; ================================================================================
;; Packages

;; Set up the "Straight" package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use use-package with straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default 1)

;; Allows to use :diminish in use-package to hide modes in the modeline
(use-package diminish)

;; Tree-sitter Language Grammars
;; Run M-x treesit-install-language-grammar to install the grammars listed here
(use-package haskell-ts-mode)
(setq treesit-language-source-alist
      '((css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1"))))

;; Replace modes with Tree-sitter modes
(setq major-mode-remap-alist
      '((css-mode . css-ts-mode)
        (web-mode . html-ts-mode)
        (html-mode . html-ts-mode)
        (js-mode . javascript-ts-mode)
        (json-mode . json-ts-mode)
        (typescript-mode . typescript-ts-mode)))

;; LSP stuff
;; ---------

;; Haskell
(use-package lsp-haskell)

;; Swift
(use-package lsp-sourcekit
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

;; LSP itself
(use-package lsp-mode
  :hook ((typescript-ts-mode . lsp-deferred)
	       (tsx-ts-mode . lsp-deferred)
         (html-ts-mode . lsp-deferred)
         (haskell-ts-mode . lsp-deferred)
         (swift-mode . lsp-deferred)
	       (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq warning-suppress-types '((lsp-mode) (lsp-mode))))

;; I think if we don't add this we will enable automatically all the clients available
;(setq lsp-enabled-clients '(deno-ls html-ls lsp-haskell))


;; Delay inline suggestions by 2 seconds
(setq completion-preview-idle-delay 2)

;; Display the undo tree
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; Version Control
(use-package magit)

;; Simultaneous editing of occurrences
(use-package iedit
  :config
  (set-face-background 'iedit-occurrence "#E1ECEB"))

;; Enable code checking
(use-package flycheck
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

;; Spell check for natural language
;; Requires Aspell
(use-package spell-fu
  :config
  (spell-fu-global-mode)
  :init
  (add-hook 'spell-fu-mode-hook
	    (lambda ()
	      (spell-fu-dictionary-add
	       (spell-fu-get-personal-dictionary "en_tech" "~/Library/Spelling/en_tech.pws"))
	      )))

;; Disable spell-checking on Dired
(add-hook 'dired-mode-hook (lambda () (spell-fu-mode -1)))

;; Make the spelling errors less prominent
(add-hook 'spell-fu-mode-hook
	  (lambda ()
	    (set-face-attribute 'spell-fu-incorrect-face nil :underline `(:color ,flexoki-yellow-200 :style wave))))

;; Show what keybindings are available after a prefix like C-x or C-c.
(setq which-key-separator " → "
      which-key-max-display-columns 1 ; We need a single column to have space for the symbol description
      which-key-popup-type 'side-window
      which-key-side-window-location 'bottom
      which-key-side-window-max-width 0.75
      which-key-max-description-length 0.9
      which-key-show-docstrings t)
(which-key-mode t)

;; Show what functions are available on M-x
(use-package vertico
  :init
  (vertico-mode))

;; Show function descriptions
(use-package marginalia
  :init
  (marginalia-mode))

;; Persist history over Emacs restarts.
;; - Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable finding functions, variables, etc. Without being precise in the spelling.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-completion-category-overrides '((file (styles basic partial-completion))))
  (setq orderless-matching-styles '(orderless-flex)))

;; .editorconfig file support
(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode +1))

;; Display current match number and total matches when searching text
(use-package anzu
  :diminish
  :config
  (global-anzu-mode t))

;; --------------------------------------------------
;; OCaml

;; Language support
(use-package tuareg
  :mode
  (("\\.ocamlinit\\'" . tuareg-mode)))

;; Check OCaml code
(use-package flycheck-ocaml
  :after flycheck
  :config
  (flycheck-ocaml-setup))

;; Build system for OCaml
(use-package dune)

;; Context sensitive completion
(use-package merlin
  :hook (tuareg-mode . merlin-mode)
  :config
  (setq merlin-error-after-save nil))

;; Documentation backed by Merlin
(use-package merlin-eldoc
  :hook (tuareg-mode . merlin-eldoc-setup)
  :config
  (set-face-background 'merlin-eldoc-occurrences-face "#EDEECF"))

;; Edit identifiers simultaneously
(use-package merlin-iedit)

;; Use the opam installed utop
(setq utop-command "opam exec -- utop -emacs")

;; Interactive Read-Eval-Print Loop
(use-package utop)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; --------------------------------------------------
;; Scheme (Chez Scheme)
(use-package geiser-chez
  :config
  (setq geiser-chez-binary "chez"))

;; --------------------------------------------------
;; Clojure
(use-package cider
  :hook (clojure-mode . cider-mode))

;; --------------------------------------------------
;; R (Programming Language)
(use-package ess)

;; --------------------------------------------------
;; Julia
(use-package julia-mode)
(use-package julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)

;; --------------------------------------------------
;; Go
(use-package go-mode)

;; --------------------------------------------------
;; D
(use-package d-mode)

;; --------------------------------------------------
;; Swift
;; Note: The standard way to develop in swift is with xcode,
;;   but it's too hard to use for me.
(use-package swift-mode)
(use-package swift-helpful) ;; Get documentation of the selected symbol

;; --------------------------------------------------
;; Fortran

;; --------------------------------------------------
;; Cobol

;; --------------------------------------------------
;; Prolog

;; --------------------------------------------------
;; Elixir

;; --------------------------------------------------
;; SML

;; --------------------------------------------------
;; Erlang

;; --------------------------------------------------
;; Typescript / Javascript and other web stuff
;; Note: Check the LSP and Tree-sitter stuff for more things related to these languages

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))

;; ================================================================================
;; Custom functions

;; Duplicate line
(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (end-of-line)
    (newline)
    (insert line)))

(defun custom-beginning-of-line ()
 "Toggle between the first non-whitespace character and the beginning of the line."
 (interactive)
 (let ((orig-point (point)))
   (back-to-indentation)
   (when (= orig-point (point))
     (move-beginning-of-line 1))))

(defun copy-to-clipboard ()
  "Copies selection to x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
	(message "Yanked region to x-clipboard!")
	(call-interactively 'clipboard-kill-ring-save))
    (if (region-active-p)
	(progn
	  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
	  (message "Yanked region to clipboard!")
	  (deactivate-mark))
      (message "No region active; can't yank to clipboard!"))))

(defun paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
	(clipboard-yank)
	(message "graphics active"))
    (insert (shell-command-to-string "pbpaste"))))

;; =============================================================================
;; General settings

;; Never use tabs for indentation
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

;; Remove mode labels in the modeline
(diminish 'completion-preview-mode)
(diminish 'eldoc-mode)
(diminish 'flycheck-mode)
(diminish 'which-key-mode)
(diminish 'lsp-lens-mode)

;; Match delimiters
(setq electric-pair-preserve-balance t)
(setq electric-pair-delete-adjacent-pairs t)
(electric-pair-mode)

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

;; IDO
;; (set-face-foreground 'ido-subdir flexoki-blue-400)
;; (set-face-foreground 'ido-first-match flexoki-green-400)
;; (set-face-foreground 'ido-only-match flexoki-green-400)
;; (set-face-background 'ido-indicator flexoki-red-50)
;; (set-face-foreground 'ido-indicator flexoki-black)

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
(set-face-attribute 'font-lock-operator-face nil :foreground flexoki-base-600 :weight 'light)
(set-face-attribute 'font-lock-punctuation-face nil :foreground flexoki-base-500)
(set-face-attribute 'font-lock-bracket-face nil :foreground flexoki-base-500)
(set-face-attribute 'font-lock-delimiter-face nil :foreground flexoki-base-500)


;; ==================================================
;; Keybindings

;; Avoid the the mistake of calling "C-x C-b" instead of "C-x b"
(global-unset-key (kbd "C-x C-b"))

;; Redefine core keybindings
(define-key global-map (kbd "C-a") '("Go to beginning of line" . custom-beginning-of-line))

;; Clipboard keybindings
(define-key global-map (kbd "C-c c") '("Copy to clipboard" . copy-to-clipboard))
(define-key global-map (kbd "C-c v") '("Paste from clipboard" . paste-from-clipboard))
(define-key global-map (kbd "C-x u") '("Display undo tree" . vundo))
(define-key global-map (kbd "C-c d") '("Duplicate line" . duplicate-line))
(define-key global-map (kbd "C-x C-r") '("Open recent file" . recentf-open))

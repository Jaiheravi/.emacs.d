;; Prevent package.el loading packages prior to their init-file loading
;;   This is a recommendation from the documentation of straight.el
(setq package-enable-at-startup nil)

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

;; Check OCaml code
(use-package flycheck-ocaml
  :after flycheck
  :config
  (flycheck-ocaml-setup))

;; Spell check for natural languages
(setq ispell-local-dictionary "en_US")
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Don't spell check hex colors
(defun my-flyspell-skip-hex-colors ()
  "Predicate to skip hexadecimal color codes during spell-checking."
  (let ((word (thing-at-point 'word t)))
    (not (and word
              (string-match-p "\\`[0-9A-Fa-f]\\{3,6\\}\\'" word)))))

(add-hook 'flyspell-mode-hook
          (lambda ()
            (setq flyspell-generic-check-word-predicate #'my-flyspell-skip-hex-colors)))

;; TODO: In Lisps, enable flyspell only on comments and strings, and not all the words inside a s-expression
;;  http://xahlee.info/emacs/emacs/elisp_thing-at-point.html

;; --------------------------------------------------
;; OCaml

(use-package tuareg
  :mode
  (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package dune)

(use-package merlin
  :hook (tuareg-mode . merlin-mode)
  :config
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :hook (tuareg-mode . merlin-eldoc-setup)
  :config
  (set-face-background 'merlin-eldoc-occurrences-face "#EDEECF"))

;; --------------------------------------------------
;; Scheme (Chez Scheme)
(use-package geiser-chez
  :config
  (setq geiser-chez-binary "chez"))

;; --------------------------------------------------
;; Clojure
(use-package cider)

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
	(call-interactively 'clipboard-kill-ring-save)
	)
    (if (region-active-p)
	(progn
	  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
	  (message "Yanked region to clipboard!")
	  (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
	(clipboard-yank)
	(message "graphics active")
	)
    (insert (shell-command-to-string "pbpaste"))
    )
  )

;; =============================================================================
;; General settings

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

;; Enable auto-completion
(global-completion-preview-mode 1)

;; Match delimiters
(electric-pair-mode)

;; ================================================================================
;; Color customization
;;   Palette from https://github.com/kepano/flexoki

(defvar flexoki-base-50 "#F2F0E5")
(defvar flexoki-base-100 "#E6E4D9")
(defvar flexoki-base-150 "#DAD8CE")
(defvar flexoki-base-200 "#CECDC3")
(defvar flexoki-base-300 "#B7B5A9")
(defvar flexoki-base-400 "#9F9D96")
(defvar flexoki-base-500 "#878580")
(defvar flexoki-base-600 "#6F6E69")
(defvar flexoki-base-700 "#575653")
(defvar flexoki-base-800 "#403E3C")
(defvar flexoki-black "#100F0F")
(defvar flexoki-red-300 "#E8705F")
(defvar flexoki-yellow-50 "#FAEEC6")

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

;; Global syntax highlighting
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

;; Redefine core keybindings
(define-key global-map (kbd "C-a") '("Go to beginning of line" . custom-beginning-of-line))

;; Clipboard keybindings
(define-key global-map (kbd "C-c c") '("Copy to clipboard" . copy-to-clipboard))
(define-key global-map (kbd "C-c v") '("Paste from clipboard" . paste-from-clipboard))
(define-key global-map (kbd "C-x u") '("Display undo tree" . vundo))
(define-key global-map (kbd "C-c d") '("Duplicate line" . duplicate-line))

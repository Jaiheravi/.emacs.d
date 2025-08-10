;; Prevent package.el loading packages prior to their init-file loading
;;   This is a recommendation from the documentation of straight.el
(setq package-enable-at-startup nil)

;; ================================================================================
;; Colors
;; Palette from https://github.com/kepano/flexoki

;; Base
(defvar flexoki-base-paper "#FFFCF0")
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
(defvar flexoki-base-850 "#343331")
(defvar flexoki-base-900 "#282726")
(defvar flexoki-base-950 "#1C1B1A")
(defvar flexoki-black "#100F0F")

;; Red
(defvar flexoki-red-50 "#FFE1D5")
(defvar flexoki-red-100 "#FFCABB")
(defvar flexoki-red-150 "#FDB2A2")
(defvar flexoki-red-200 "#F89A8A")
(defvar flexoki-red-300 "#E8705F")
(defvar flexoki-red-400 "#D14D41")
(defvar flexoki-red-500 "#C03E35")
(defvar flexoki-red-600 "#AF3029")
(defvar flexoki-red-700 "#942822")
(defvar flexoki-red-800 "#6C201C")
(defvar flexoki-red-850 "#551B18")
(defvar flexoki-red-900 "#3E1715")
(defvar flexoki-red-950 "#261312")

;; Orange
(defvar flexoki-orange-50 "#FFE7CE")
(defvar flexoki-orange-100 "#FED3AF")
(defvar flexoki-orange-150 "#FCC192")
(defvar flexoki-orange-200 "#F9AE77")
(defvar flexoki-orange-300 "#EC8B49")
(defvar flexoki-orange-400 "#DA702C")
(defvar flexoki-orange-500 "#CB6120")
(defvar flexoki-orange-600 "#BC5215")
(defvar flexoki-orange-700 "#9D4310")
(defvar flexoki-orange-800 "#71320D")
(defvar flexoki-orange-850 "#59290D")
(defvar flexoki-orange-900 "#40200D")
(defvar flexoki-orange-950 "#27180E")

;; Yellow
(defvar flexoki-yellow-50 "#FAEEC6")
(defvar flexoki-yellow-100 "#F6E2A0")
(defvar flexoki-yellow-150 "#F1D67E")
(defvar flexoki-yellow-200 "#ECCB60")
(defvar flexoki-yellow-300 "#DFB431")
(defvar flexoki-yellow-400 "#D0A215")
(defvar flexoki-yellow-500 "#BE9207")
(defvar flexoki-yellow-600 "#AD8301")
(defvar flexoki-yellow-700 "#8E6B01")
(defvar flexoki-yellow-800 "#664D01")
(defvar flexoki-yellow-850 "#503D02")
(defvar flexoki-yellow-900 "#3A2D04")
(defvar flexoki-yellow-950 "#241E08")

; Green
(defvar flexoki-green-50 "#EDEECF")
(defvar flexoki-green-100 "#DDE2B2")
(defvar flexoki-green-150 "#CDD597")
(defvar flexoki-green-200 "#BEC97E")
(defvar flexoki-green-300 "#A0AF54")
(defvar flexoki-green-400 "#879A39")
(defvar flexoki-green-500 "#768D21")
(defvar flexoki-green-600 "#66800B")
(defvar flexoki-green-700 "#536907")
(defvar flexoki-green-800 "#3D4C07")
(defvar flexoki-green-850 "#313D07")
(defvar flexoki-green-900 "#252D09")
(defvar flexoki-green-950 "#1A1E0C")

;; Cyan
(defvar flexoki-cyan-50 "#DDF1E4")
(defvar flexoki-cyan-100 "#BFE8D9")
(defvar flexoki-cyan-150 "#A2DECE")
(defvar flexoki-cyan-200 "#87D3C3")
(defvar flexoki-cyan-300 "#5ABDAC")
(defvar flexoki-cyan-400 "#3AA99F")
(defvar flexoki-cyan-500 "#2F968D")
(defvar flexoki-cyan-600 "#24837B")
(defvar flexoki-cyan-700 "#1C6C66")
(defvar flexoki-cyan-800 "#164F4A")
(defvar flexoki-cyan-850 "#143F3C")
(defvar flexoki-cyan-900 "#122F2C")
(defvar flexoki-cyan-950 "#101F1D")

;; Blue
(defvar flexoki-blue-50 "#E1ECEB")
(defvar flexoki-blue-100 "#C6DDE8")
(defvar flexoki-blue-150 "#ABCFE2")
(defvar flexoki-blue-200 "#92BFDB")
(defvar flexoki-blue-300 "#66A0C8")
(defvar flexoki-blue-400 "#4385BE")
(defvar flexoki-blue-500 "#3171B2")
(defvar flexoki-blue-600 "#205EA6")
(defvar flexoki-blue-700 "#1A4F8C")
(defvar flexoki-blue-800 "#163B66")
(defvar flexoki-blue-850 "#133051")
(defvar flexoki-blue-900 "#12253B")
(defvar flexoki-blue-950 "#101A24")

;; Purple
(defvar flexoki-purple-50 "#F0EAEC")
(defvar flexoki-purple-100 "#E2D9E9")
(defvar flexoki-purple-150 "#D3CAE6")
(defvar flexoki-purple-200 "#C4B9E0")
(defvar flexoki-purple-300 "#A699D0")
(defvar flexoki-purple-400 "#8B7EC8")
(defvar flexoki-purple-500 "#735EB5")
(defvar flexoki-purple-600 "#5E409D")
(defvar flexoki-purple-700 "#4F3685")
(defvar flexoki-purple-800 "#3C2A62")
(defvar flexoki-purple-850 "#31234E")
(defvar flexoki-purple-900 "#261C39")
(defvar flexoki-purple-950 "#1A1623")

;; Magenta
(defvar flexoki-magenta-50 "#FEE4E5")
(defvar flexoki-magenta-100 "#FCCFDA")
(defvar flexoki-magenta-150 "#F9B9CF")
(defvar flexoki-magenta-200 "#F4A4C2")
(defvar flexoki-magenta-300 "#E47DA8")
(defvar flexoki-magenta-400 "#CE5D97")
(defvar flexoki-magenta-500 "#B74583")
(defvar flexoki-magenta-600 "#A02F6F")
(defvar flexoki-magenta-700 "#87285E")
(defvar flexoki-magenta-800 "#641F46")
(defvar flexoki-magenta-850 "#4F1B39")
(defvar flexoki-magenta-900 "#39172B")
(defvar flexoki-magenta-950 "#24131D")


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
(setq ispell-program-name "hunspell")
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
            (setq flyspell-generic-check-word-predicate #'my-flyspell-skip-hex-colors)
	    (set-face-attribute 'flyspell-duplicate nil :underline `(:color ,flexoki-yellow-150 :style wave))
	    (set-face-attribute 'flyspell-incorrect nil :underline `(:color ,flexoki-yellow-200 :style wave))
	    (setq ispell-local-dictionary "en_US,en_tech")
	    (ispell-hunspell-add-multi-dic "en_US,en_tech")))

;; TODO: In Lisps, enable flyspell only on comments and strings, and not all the words inside an s-expression
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
(global-completion-preview-mode)
(setq ido-enable-flex-matching t)
(setq ido-use-url-at-point t)
(setq ido-case-fold t)
(setq ido-everywhere t)
(setq ido-use-faces t)
(ido-mode t)

;; Match delimiters
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
(set-face-foreground 'ido-subdir flexoki-blue-400)
(set-face-foreground 'ido-first-match flexoki-green-400)
(set-face-foreground 'ido-only-match flexoki-green-400)
(set-face-background 'ido-indicator flexoki-red-50)
(set-face-foreground 'ido-indicator flexoki-black)

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

;; Redefine core keybindings
(define-key global-map (kbd "C-a") '("Go to beginning of line" . custom-beginning-of-line))

;; Clipboard keybindings
(define-key global-map (kbd "C-c c") '("Copy to clipboard" . copy-to-clipboard))
(define-key global-map (kbd "C-c v") '("Paste from clipboard" . paste-from-clipboard))
(define-key global-map (kbd "C-x u") '("Display undo tree" . vundo))
(define-key global-map (kbd "C-c d") '("Duplicate line" . duplicate-line))

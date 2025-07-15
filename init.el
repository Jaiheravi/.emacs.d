;; Don't show a startup creen
(setq inhibit-startup-screen t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))

;; ==================================================
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

;; Display the undo tree
(straight-use-package 'vundo)
(setq vundo-glyph-alist vundo-unicode-symbols)

;; Autocompletion
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Elixir setup
(straight-use-package 'elixir-mode)
(require 'eglot)
(add-hook 'elixir-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(elixir-mode "~/.emacs.d/elixir-ls/release/language_server.sh"))

(straight-use-package 'markdown-mode)

(setq flymake-fringe-indicator-position 'left-fringe)
(setq flymake-error-bitmap '(exclamation-mark error))
(setq eglot-workspace-configuration
      '(:elixirLS (:projectDir (:root-uri))))
(setq eglot-workspace-configuration
      '(:elixirLS (:dialyzerEnabled t)))

(straight-use-package 'projectile)
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; =============================================================================
;; General settings

;; Hide menu bar
(menu-bar-mode -1)


;; Ask for Y or N instead of Yes or No
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remove code highlighting
(global-font-lock-mode -1)

;; Display line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Display column numbers
(setq column-number-mode t)

;; Insert matching delimiters
(electric-pair-mode)

;; Color customizations
(set-face-background 'mode-line "#F3F1E5")
(set-face-background 'mode-line-inactive "#FEFCF0")
(set-face-background 'show-paren-match "#FEFCF0")
(set-face-foreground 'show-paren-match "#E58C8A")
(set-face-foreground 'line-number-current-line "#E58C8A")
(set-face-foreground 'line-number "#D5D2C1")

;; Make sure there is always a new line at the end of the file
(setq require-final-newline t)

;; Remove trailing whitespace when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Remove the dash separator in the mode line
(setq-default mode-line-end-spaces "")

;; Enable autocompletion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Duplicate line
(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (end-of-line)
    (newline)
    (insert line)))


;; ==================================================
;; Functions


(defun metaverso-beginning-of-line ()
  "Toggle between the first non-whitespace character and the beginning of the line"
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




;; ==================================================
;; Keybindings

;; Redefine core keybindings
(define-key global-map (kbd "C-a") '("Go to beginning of line" . metaverso-beginning-of-line))

;; Clipboard keybindings
(define-key global-map (kbd "C-c c") '("Copy to clipboard" . copy-to-clipboard))
(define-key global-map (kbd "C-c v") '("Paste from clipboard" . paste-from-clipboard))
(define-key global-map (kbd "C-x u") '("Display undo tree" . vundo))
(define-key global-map (kbd "C-c d") '("Duplicate line" . duplicate-line))

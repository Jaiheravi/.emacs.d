;;; -*- lexical-binding: t -*-

(add-hook 'prog-mode-hook (lambda ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2)))

(setq js-indent-level 2)

(add-hook 'css-ts-mode-hook '(lambda ()
  (setq indent-tabs-mode nil)
  (setq css-indent-offset 2)
  (setq css-fontify-colors nil)
))

;; --------------------------------------------------
;; Tree-sitter Parsers
;; Run treesit-install-language-grammar
;;
;; Because apparently tree-sitter parsers are a lot faster
;; than the classic ones written in Elisp.

(setq treesit-language-source-alist
      '((html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (json "https://github.com/tree-sitter/tree-sitter-json")))

;; Evaluate to install grammars
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; (use-package haskell-ts-mode
;;   :custom
;;   (haskell-ts-use-indent t)
;;   :vc (:url "git@github.com:Jaiheravi/haskell-ts-mode.git"
;;             :rev :newest))

(add-hook 'js-mode-hook #'js-ts-mode)
(add-hook 'html-mode-hook #'html-ts-mode)
(add-hook 'json-mode-hook #'json-ts-mode)
(add-hook 'haskell-mode-hook #'haskell-ts-mode)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))

;; --------------------------------------------------
;; Language Server Protocol
;; Provides IDE-like language integration

;; Haskell
(use-package
  lsp-haskell
  :ensure t
  :defer t)

;; LSP itself
(use-package lsp-mode
  :ensure t
  ;;:delight (lsp-mode " LSP" lsp-lens-mode)
  :delight (lsp-mode " LSP")
  :defer t
  :hook ((js-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (css-ts-mode . lsp-deferred)
         (haskell-ts-mode . lsp-deferred)
	       (lsp-mode . lsp-enable-which-key-integration)
         (swift-mode . lsp))
  :commands lsp
  :init
  (setq lsp-css-color-decorations nil)
  :config
  ;;(setq lsp-lens-enable nil) ; A bit too intrusive
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-code-actions-segments '(count))  ; Show count only, no icon
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq warning-suppress-types '((lsp-mode) (lsp-mode)))
  (setq lsp-eslint-enable nil))

;; --------------------------------------------------
;; Scheme (Chez Scheme)
(use-package geiser-chez
  :ensure t
  :defer t
  :config
  (setq geiser-chez-binary "chez"))

;; --------------------------------------------------
;; OCaml
(add-to-list 'load-path "/Users/jaime/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; --------------------------------------------------
;; Swift

(defun custom/find-sourcekit-lsp () ; Locate sourcekit-lsp
  (or (executable-find "sourcekit-lsp")
      (and (eq system-type 'darwin)
           (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
      "/Library/Developer/CommandLineTools/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :ensure t
  :defer t
  :mode "\\.swift\\'"
  :interpreter "swift"
  :config
  (setq swift-mode:basic-offset 2))

(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :custom
  (lsp-sourcekit-executable (custom/find-sourcekit-lsp) "Find sourcekit-lsp"))

(use-package swift-helpful
  :ensure t
  :after lsp-mode
  :bind ("C-c h" . swift-helpful))

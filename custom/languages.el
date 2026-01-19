(add-hook 'prog-mode-hook (lambda ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2)))

(setq js-indent-level 2)

;; --------------------------------------------------
;; Tree-sitter Parsers
;; Run treesit-install-language-grammar
;;
;; Because apparently tree-sitter parsers are a lot faster
;; than the classic ones written in Elisp.

(setq treesit-language-source-alist
      '((html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (haskell "https://github.com/tree-sitter/haskell-tree-sitter")))

;; (use-package haskell-ts-mode
;;   :custom
;;   (haskell-ts-use-indent t)
;;   :vc (:url "git@github.com:Jaiheravi/haskell-ts-mode.git"
;;             :rev :newest))

(add-hook 'js-mode-hook #'js-ts-mode)
(add-hook 'html-mode-hook #'html-ts-mode)
(add-hook 'json-mode-hook #'json-ts-mode)
(add-hook 'haskell-mode-hook #'haskell-ts-mode)

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
  :defer t
  :hook ((js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (haskell-ts-mode . lsp-deferred)
	       (lsp-mode . lsp-enable-which-key-integration)
         (swift-mode . lsp))
  :commands lsp
  ;;:custom
  ;;(setq lsp-enabled-clients '(lsp-haskell))
  :config
  (setq lsp-lens-enable nil) ; A bit too intrusive
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq warning-suppress-types '((lsp-mode) (lsp-mode))))

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
  :interpreter "swift")

(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :custom
  (lsp-sourcekit-executable (custom/find-sourcekit-lsp) "Find sourcekit-lsp"))

(use-package swift-helpful
  :ensure t
  :after lsp-mode
  :bind ("C-c h" . swift-helpful))

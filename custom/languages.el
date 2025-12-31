;; --------------------------------------------------
;; Tree-sitter Parsers
;; Run treesit-install-language-grammar
;;
;; Because apparently tree-sitter parsers are a lot faster
;; than the classic ones written in Elisp.

(setq treesit-language-source-alist
      '((haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1"))))

(use-package haskell-ts-mode
  :custom
  (haskell-ts-use-indent t)
  :vc (:url "git@github.com:Jaiheravi/haskell-ts-mode.git"
            :rev :newest))

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
  :hook ((haskell-ts-mode . lsp-deferred)
	       (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (setq lsp-enabled-clients '(lsp-haskell))
  :config
  (setq lsp-lens-enable nil) ; A bit too intrusive
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq warning-suppress-types '((lsp-mode) (lsp-mode)))
  :defer t)

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

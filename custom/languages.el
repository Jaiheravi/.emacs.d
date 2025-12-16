;; Tree-sitter Language Grammars
;; Run M-x treesit-install-language-grammar to install the grammars listed here
(use-package haskell-ts-mode
  :vc (:url "git@github.com:Jaiheravi/haskell-ts-mode.git"
       :rev :newest))

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
         (css-ts-mode . lsp-deferred)
         (haskell-ts-mode . lsp-deferred)
         (swift-mode . lsp-deferred)
	       (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-lens-enable nil) ; A bit too intrusive
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq warning-suppress-types '((lsp-mode) (lsp-mode))))

(setq lsp-enabled-clients '(deno-ls html-ls css-ls lsp-haskell))


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

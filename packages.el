;;; packages.el --- Package setup -*- lexical-binding: t; -*-

(require 'config)

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(when (require 'package-vc nil 'noerror)
  (setq use-package-vc-prefer-newest t))
(setq use-package-always-ensure t)

(use-package anaphora :demand t)
(use-package dash :demand t)

(require 'gptel-tweaks)

(use-package ag)

(use-package aweshell
  :vc (:url "https://github.com/manateelazycat/aweshell.git")
  :bind ("C-c x" . #'aweshell-dedicated-toggle))

(use-package vterm
  :commands vterm)

(use-package codex-cli
  :vc (:url "https://github.com/bennfocus/codex-cli.el.git"))

(use-package blacken
  :ensure t
  :config
  (if (on-pinely-host)
      (setq blacken-executable "/home/bkc/twix-black.sh")
    (setq blacken-line-length 80))
  :hook (python-mode . blacken-mode))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.3))

(use-package company-coq
  :after (proof-general)
  :hook (coq-mode . company-coq-mode)
  :init (setq company-coq-live-on-the-edge t))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'lsp-deferred)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (define-key cider-mode-map (kbd "C-c RET") nil)
  (define-key cider-mode-map
              (kbd "C-c C-r RET")
              'cider-macroexpand-1))

(use-package cmake-mode)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package desert-theme
  :vc (:url "https://github.com/bkc39/desert-theme.git")
  :config
  (load-theme 'desert t t)
  (enable-theme 'desert))

(use-package ein)

(use-package eshell-prompt-extras
  :config
  (setq eshell-prompt-function 'epe-theme-lambda))

(use-package ess
  :mode ("\\.R\\'" . R-mode))

(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns))
          (memq system-type '(darwin gnu/linux)))
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :hook (emacs-lisp-mode . flycheck-mode))

(use-package go-mode
  :after lsp-mode
  :hook (go-mode . (lambda ()
                     (electric-pair-mode 1)))
  :config
  (if (executable-find "gopls")
      (add-hookq go-mode-hook #'lsp-deferred)
    (message "go-mode LSP plugin gopls is not installed!")))

(use-package gptel
  :ensure t
  :config
  (setq gptel-model "gpt-5-mini"
        gptel-stream nil
        gptel-api-key (get-openai-api-key))
  (ensure-gptel-directives-loaded)
  (setq-default
   gptel--system-message
   (alist-get 'default gptel-directives "You are a helpful assistant."))
  (gptel-make-ollama
      "Ollama"
    :host "localhost:11434"
    :models '("gemma2:27b" "llama")
    :stream t)
  (gptel-make-anthropic "Claude"
    :stream t
    :key (get-anthropic-api-key))
  :bind (("C-c RET" . gptel-send)
         ("C-c q" . gptel-quick)
         ("C-c M-d" . gptel-diff)
         ("C-c M-p" . gptel-pull-request)
         ("C-c M-s" . gptel-document-symbol-at-point)
         ("C-c M-t" . gptel-tests-for-symbol-at-point)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :custom
  (lsp-file-watch-ignored
   '("[/\\\\]\\.venv$" "[/\\\\]venv$" "[/\\\\]\\.direnv$"
     "[/\\\\]\\.mypy_cache$" "[/\\\\]\\.pytest_cache$"
     "[/\\\\]__pycache__$" "[/\\\\]build$" "[/\\\\]dist$"))
  :config
  (setq lsp-enable-file-watchers nil
        lsp-file-watch-threshold 10000))

(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook
  ((python-mode . lsp-deferred)
   (before-save . python-buffer-whitespace-cleanup)
   (before-save . organize-python-imports)))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (on-macos
   (setq lsp-sourcekit-executable
         (string-trim
          (shell-command-to-string
           "xcrun --find sourcekit-lsp")))))

(use-package magit
  :bind (("C-c m" . magit-status))
  :hook ((git-commit-mode . insert-issue-prefix)))

(use-package multiple-cursors
  :bind (("C-c M-c" . mc/edit-lines)))

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-mode-hook #'enable-paredit-mode)
  :bind
  (:map paredit-mode-map
        ("{"     . paredit-open-curly)
        ("}"     . paredit-close-curly)
        ("C-M-[" . paredit-forward-slurp-sexp)
        ("C-M-]" . paredit-forward-barf-sexp)))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (display-line-numbers-mode -1))))

(use-package proof-general
  :hook (coq-mode . prettify-symbols-mode)
  :custom
  (coq-prog-args
   (list
    "-emacs"
    "-R"
    (or (getenv "CPDT_SRC_DIR")
        "/Users/bkc/dev/coq/cpdt/src")
    "Cpdt")))

(use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :mode ("\\.scrbl\\'" . racket-hash-lang-mode)
  :hook (racket-mode . racket-unicode-input-method-enable)
  :config
  (with-temp-buffer
    (racket-unicode-input-method-enable)
    (let ((quail-current-package (assoc "racket-unicode"
                                        quail-package-alist)))
      (quail-define-rules
       ((append . t))
       ("oplus" ["⊕"])
       ("otimes" ["⊗"])))))

(use-package rust-mode
  :hook (rust-mode . (lambda ()
                       (electric-pair-mode 1)))
  :custom
  (rust-format-on-save t)
  :bind (("C-c C-c m l" . #'lsp-rust-analyzer-open-cargo-toml)
         ("C-c C-c C-n" . #'rust-run-with-args)))

(use-package react-snippets)

(use-package swift-mode
  :hook (swift-mode . #'lsp-deferred))

(use-package solidity-mode)

(use-package web-mode
  :mode ("\\.tsx\\'" . web-mode)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package whitespace
  :config
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face lines trailing empty))
  :hook (prog-mode . whitespace-mode)
  :hook (before-save . whitespace-cleanup))

(use-package yasnippet
  :hook (js-mode . yas-minor-mode)
  :hook (lsp-mode . yas-minor-mode))

(use-package zenburn-theme)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el.git")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-c C-n" . copilot-next-completion)
              ("C-c C-p" . copilot-previous-completion))
  :config
  (setq copilot-idle-delay 3.0)
  (setq copilot-indent-offset-warning-disable t))

(provide 'packages)

;;; packages.el ends here

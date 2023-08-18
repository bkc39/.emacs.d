(defmacro on-system (system &rest body)
  `(when (eq system-type ',system)
     ,@body))

(defmacro on-macos (&rest body)
  `(on-system 'darwin ,@body))

(defmacro on-linux (&rest body)
  `(on-system 'gnu/linux ,@body))

(defmacro add-hookq (hook-name fn)
  "Like add-hook, but automatically quotes the hook-name"
  `(add-hook ',hook-name ,fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight.el config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; configure straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat
          "https://raw.githubusercontent.com"
          "/radian-software/straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ag)

(use-package company-coq
  :after (proof-general)
  :hook (coq-mode . company-coq-mode)
  :init (setq company-coq-live-on-the-edge t))

(use-package cmake-mode)

(use-package ein)

(use-package ess
  :mode ("\\.R\\'" . R-mode))

(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns))
          (eq system-type 'darwin))
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck)

(use-package shellgpt
  :straight (:host github
             :repo "bkc39/shellgpt.el"
             :files ("dist" "*.el"))
  :bind ("C-c q" . shellgpt:quick-ask)
  :custom
  (shellgpt:repl-chat-name
   (concat "emacs-"
           (format-time-string "%Y%m%d"
                               (current-time)))))

(use-package go-mode
  :after lsp-mode
  :hook (go-mode . (lambda ()
                     (electric-pair-mode 1)))
  :config
  (if (executable-find "gopls")
      (progn
        (add-hookq go-mode-hook #'lsp-deferred)
        (add-hookq before-save-hook #'lsp-format-buffer)
        (add-hookq before-save-hook #'lsp-organize-imports))
    (message "go-mode LSP plugin gopls is not installed!")))


(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :hook (js-mode . (lambda ()
                     (electric-indent-mode -1)
                     (lsp)))
  :hook (rust-mode . #'lsp-deferred))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config
  (progn
    (setq lsp-pyright-use-library-code-for-types t)
    (let* ((pyright-stubs-root-dir
            (getenv "PYRIGHT_TYPE_STUBS_ROOT"))
           (pyright-stubs-dir
            (concat pyright-stubs-root-dir
                    "/python-type-stubs")))
      (when (and pyright-stubs-root-dir
                 pyright-stubs-dir)
        (setq lsp-pyright-stubs-path
              pyright-stubs-dir)))))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (on-macos
   (setq lsp-sourcekit-executable
         (string-trim
          (shell-command-to-string
           "xcrun --find sourcekit-lsp")))))

(use-package magit
  :bind (("C-c m" . magit-status)))

(use-package multiple-cursors
  :bind (("C-c C-m C-c" . mc/edit-lines)))

(use-package paredit
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook #'enable-paredit-mode)
    (add-hook 'racket-mode-hook #'enable-paredit-mode))
  :bind
  (:map paredit-mode-map
        ("{"     . paredit-open-curly)
        ("}"     . paredit-close-curly)
        ("C-M-[" . paredit-forward-slurp-sexp)
        ("C-M-]" . paredit-forward-barf-sexp)))

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
  :hook (racket-mode . (lambda ()
                         (racket-xp-mode t)))
  :config
  (setq racket-command-port 9091)
  (racket-unicode-input-method-enable)
  (add-hookq racket-xp-mode-hook
             (lambda ()
               (remove-hook 'pre-redisplay-functions
                            #'racket-xp-pre-redisplay
                            t))))

(use-package rust-mode
  :hook (rust-mode . (lambda ()
                       (electric-pair-mode 1)))
  :custom
  (rust-format-on-save t)
  :bind ("C-c C-c m l" . #'lsp-rust-analyzer-open-cargo-toml))

(use-package react-snippets)

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

(use-package solidity-mode)
(use-package tree-sitter)
(use-package tree-sitter-langs)

(defun setup-tide-mode ()
  "hook to setup tide"
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package web-mode
  :mode ("\\.tsx\\'" . web-mode)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package whitespace
  :init
  (add-hook 'before-save-hook
            'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines))
  :hook (prog-mode . whitespace-mode))

(use-package yasnippet
  :hook (js-mode . yas-minor-mode)
  :hook (lsp-mode . yas-minor-mode))

(use-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t t)
    (enable-theme 'zenburn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other configs

(if (<= 29 (car (version-to-list emacs-version)))
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))
(setq column-number-mode t)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

;; set the font to anonymous pro when in GUI if its installed
(when (and (display-graphic-p)
           (x-list-fonts "Anonymous Pro"))
  (add-to-list 'default-frame-alist
               '(font . "Anonymous Pro-12")))

(setq js-indent-level 2)
(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))

(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))

(add-hookq
 after-change-major-mode-hook
 #'(lambda ()
     (electric-indent-mode -1)))

;; open up the init file in the background on startup
(add-hookq
 emacs-startup-hook
 (lambda ()
   (find-file-noselect user-init-file)))

(defun restart-server ()
  "Restart the emacs server and close all clients"
  (interactive)
  (if server-process
      (progn
        (server-force-delete)
        (server-start))
    (message "Not in a client. Exiting...")
    'ok))

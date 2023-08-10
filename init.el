(defmacro on-system (system &rest body)
  `(when (eq system-type ',system)
     ,@body))

(defmacro on-macos (&rest body)
  `(on-system 'darwin ,@body))

(defmacro on-linux (&rest body)
  `(on-system 'gnu/linux ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straigth.el config
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

(use-package chatgpt
  :straight (:host github
                   :repo "joshcho/ChatGPT.el"
                   :files ("dist" "*.el"))

  :bind ("C-c q" . chatgpt-query)
  :init
  ;; default to the environment variable
  (unless (getenv "OPENAI_API_KEY")
    ;; otherwise read the api key in from the config file
    (setq openai-config-file "~/.openai")
    (message "OPENAI_API_KEY not defined. Attempting to read from file: %s"
             openai-config-file)
    (if (file-exists-p openai-config-file)
        (let ((config-file-sk
               (with-temp-buffer
                 (insert-file-contents openai-config-file)
                 (buffer-string))))
          (setenv "OPENAI_API_KEY" config-file-sk))
      (message "openai config file does not exist. Exiting...")))
  :config
  (setq chatgpt-cli-file-path
        (executable-find "lwe")))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :hook (js-mode . (lambda ()
                     (electric-indent-mode -1)
                     (lsp))))

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
   (message "I did this")
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
  :config
  (setq racket-command-port 9091)
  (racket-unicode-input-method-enable))

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
  :hook (js-mode . yas-minor-mode-on))

(use-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t t)
    (enable-theme 'zenburn)))

(global-linum-mode 1)
(setq column-number-mode t)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))
(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))
(add-hook
 'after-change-major-mode-hook
 #'(lambda ()
     (electric-indent-mode -1)))
(setq js-indent-level 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cmake-mode ein bind-key company company-math compat f git-commit ht lv magit-section markdown-mode math-symbol-lists s spinner transient with-editor yasnippet zenburn-theme swift-mode racket-mode proof-general paredit multiple-cursors magit lsp-sourcekit lsp-pyright lsp-mode ess exec-path-from-shell company-coq dash use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

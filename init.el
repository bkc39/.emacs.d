(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(defmacro on-system (system &rest body)
  `(when (eq system-type ',system)
     ,@body))

(defmacro on-macos (&rest body)

  `(on-system 'darwin ,@body))

(defmacro on-linux (&rest body)
  `(on-system 'gnu/linux ,@body))

(unless (package-installed-p 'use-package)
  (message "%s" "Refreshing package set")
  (package-refresh-contents)
  (message "%s" "installing use-package")
  (package-install 'use-package)
  (message "%s" "done!"))

(unless (package-installed-p 'dash)
  (package-refresh-contents)
  (package-install 'dash))

(eval-when-compile
  (require 'use-package))

(use-package company-coq
  :ensure t
  :after (proof-general)
  :hook (coq-mode . company-coq-mode)
  :init (setq company-coq-live-on-the-edge t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package ess
  :ensure t
  :mode ("\\.R\\'" . R-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package lsp-pyright
  :ensure t
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
  :ensure t
  :after lsp-mode
  :config
  (on-macos
   (message "I did this")
   (setq lsp-sourcekit-executable
         (string-trim
          (shell-command-to-string
           "xcrun --find sourcekit-lsp")))))

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-m C-c" . mc/edit-lines)))

(use-package paredit
  :ensure t
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
  :ensure t
  :hook (coq-mode . prettify-symbols-mode)
  :custom
  (coq-prog-args
   '("-R" "/Users/bkc/dev/coq/cpdt/src" "Cpdt")
   "Add the Cpdt libraries"))

(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode)
  :config
  (setq racket-command-port 9091))

(use-package swift-mode
  :ensure t
  :hook (swift-mode . (lambda () (lsp))))

(use-package whitespace
  :ensure t
  :init
  (add-hook 'before-save-hook
            'whitespace-cleanup))

(use-package zenburn-theme
  :ensure t
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zenburn-theme swift-mode racket-mode proof-general paredit multiple-cursors magit lsp-sourcekit lsp-pyright lsp-mode ess exec-path-from-shell company-coq dash use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

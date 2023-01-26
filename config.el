(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (message "%s" "Refreshing package set")
  (package-refresh-contents)
  (message "%s" "installing use-package")
  (package-install 'use-package)
  (message "%s" "done!"))

(eval-when-compile
  (require 'use-package))

(use-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t t)
    (enable-theme 'zenburn)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package haskell-mode
  :mode "\\.hs$"
  :init
  (progn
    (setq haskell-process-args-ghci
          '("-ferror-spans" "-fshow-loaded-modules"))
    (setq haskell-process-args-cabal-repl
          '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
    (setq haskell-process-args-stack-ghci
          '("--ghci-options=-ferror-spans"
            " -fshow-loaded-modules"
            "--no-build"
            "--no-load")))
  :bind (:map haskell-mode-map
              ("C-c C-l" . haskell-process-load-file)
              ("C-c C-z" . haskell-interactive-switch)
              ("C-`" . haskell-interactive-bring)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-<right>" . (lambda ()
                                   (interactive)
                                   (haskell-move-nested 1)))
              ("C-c C-<left>" . (lambda ()
                                  (interactive)
                                  (haskell-mode-nested -1))))
  :config
  (progn
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-ghc)))
  :custom
  (company-ghc-show-info t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-tags-on-save t)
  (haskell-process-suggest-hoogle-imports t)
  (haskell-process-type 'stack-ghci)
  (haskell-process-path-ghci "stack"))

(eval-and-compile
  (defun ess-site-load-path ()
    (shell-command "find ~ -path ess/lisp")))

(use-package ess-site
  :load-path (lambda () (list (ess-site-load-path)))
  :commands R)

(use-package ess
  :mode ("\\.R\\'" . R-mode)
  :init
  (progn
    (autoload 'R-mode "ess-site.el" "" t)
    (setq ess-indent-level 2))
  :config
  (progn
    (local-unset-key "_")
    (ess-toggle-underscore nil))
  :custom
  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers  . t)
     (ess-R-fl-keyword:fun-defs   . t)
     (ess-R-fl-keyword:keywords   . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants  . t)
     (ess-fl-keyword:fun-calls    . t)
     (ess-fl-keyword:numbers      . t)
     (ess-fl-keyword:operators    . t)
     (ess-fl-keyword:delimiters   . t)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T))))

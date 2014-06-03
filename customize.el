(load "~/.emacs.d/pkg.el")

;; Sets the PATH environment variable
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Binds 'git status' to C-c m
(require 'magit)
(define-key global-map (kbd "C-C m") 'magit-status)

;; Starts up yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (add-hook 'term-mode-hook
;; 	  (lambda ()
;; 	    (setq yas-dont-activate t)))

;; Initialize Tuareg-mode for OCAML
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; Custom OCaml stuff
(add-hook 'tuareg-mode-hook
          '(lambda ()
             ;; pressing "RETURN" also indents
             (local-set-key (kbd "RET") 'newline-and-indent)

             ;; clean up whitespace at save
             (require 'whitespace)
             (make-local-variable 'before-save-hook)
             (add-hook 'before-save-hook 'whitespace-cleanup)

	     ;; fun is lambda
             (font-lock-add-keywords
              nil `(("(\\(fun\\>\\)"
                     (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                               ,(make-char 'greek-iso8859-7 235))
                               nil)))))))

;; Clojure
(autoload 'clojure-mode "clojure-mode"
  "Major mode for editing Clojure files" t)
(setq auto-mode-alist
      (append '(("\\.clj$" . clojure-mode))
              auto-mode-alist))

;; Scala
(autoload 'scala-mode2 "scala-mode2"
  "Major mode for editing Scala files" t)
(setq auto-mode-alist
      (append '(("\\.scala$" . scala-mode2))
              auto-mode-alist))

;; Haskell
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell files" t)
(setq auto-mode-alist
      (append '(("\\.hs$" . haskell-mode)
                ("\\.lhs$" . haskell-mode))
              auto-mode-alist))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
(add-hook 'haskell-mode-hook '(lambda ()
				(require 'haskell-mode)
				(define-key haskell-mode-map "C-c h" 'haskell-hoogle)
				(setq haskell-hoogle-command "hoogle")))

(require 'ess)

(require 'color-theme)
(color-theme-initialize)
(load-theme 'zenburn t)

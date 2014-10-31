;;; customize.el --- package customizations
;;; Commentary:
;;;   where all my language-specific packages are configured

;;; Code:
(load "~/.emacs.d/pkg.el")

;; Sets the PATH environment variable
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Binds 'git status' to C-c m
(define-key global-map (kbd "C-C m") 'magit-status)

;; Starts up yasnippet
;; (yas-global-mode 0)
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

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook
          '(lambda ()
             ;; Flycheck
             (flycheck-select-checker 'haskell-hlint)
             ;; Set up hoogle
             (setq haskell-hoogle-command "hoogle")
             (define-key haskell-mode-map (kbd "M-[") 'align)
             (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
             ))


;; Flycheck stuff
(add-hook 'after-init-hook #'global-flycheck-mode)

;; For indenting entire code blocks
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

;; For Interactive Haskell Mode
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log)
 '(haskell-tags-on-save t)
 '(haskell-stylish-on-save t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-type 'cabal-repl))

;; REPL keyboard shortcuts
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
 
;; AUCTeX
(defvar TeX-auto-save)
(defvar TeX-parse-self)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; LaTeX
(defvar reftex-plug-into-AUCTeX)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Make latexmk available via C-c C-c
;; Note: SyncTeX is setup via $HOME/.latexmkrc
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)
            (push
             '("pdflatex" "pdflatex %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)))

(add-hook 'TeX-mode-hook
          '(lambda ()
             (setq TeX-command-default "pdflatex")))

;; Set Preview as the default pdf viewer
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer"
         "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (when buffer-file-name
              (add-hook 'after-save-hook
                        'check-parens
                        nil t))))

;; R
(custom-set-variables
 '(ess-R-font-lock-keywords
   (quote ((ess-R-fl-keyword:modifiers  . t)
           (ess-R-fl-keyword:fun-defs   . t)
           (ess-R-fl-keyword:keywords   . t)
           (ess-R-fl-keyword:assign-ops . t)
           (ess-R-fl-keyword:constants  . t)
           (ess-fl-keyword:fun-calls    . t)
           (ess-fl-keyword:numbers      . t)
           (ess-fl-keyword:operators    . t)
           (ess-fl-keyword:delimiters   . t)
           (ess-fl-keyword:=)
           (ess-R-fl-keyword:F&T)))))

;; Color theme
(color-theme-initialize)
(load-theme 'zenburn t)

(provide 'customize)
;;; customize.el ends here

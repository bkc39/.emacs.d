;;; setup-haskell.el --- Setup for Haskell mode
;;; Commentary:
;;;   Tweaks to the default haskell-mode

;;; Code:

(require 'haskell-interactive-mode)
(require 'haskell-process)

;; Haskell
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell files" t)

;; ;; structured haskell mode
;; (add-to-list
;;  'load-path
;;  (expand-file-name "~/.emacs.d/elisp/structured-haskell-mode/elisp"))

;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(setq auto-mode-alist
      (append '(("\\.hs$" . haskell-mode)
                ("\\.lhs$" . haskell-mode))
              auto-mode-alist))

(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Haskell main editing mode key bindings.
(defun haskell-hook ()
  ;; Keymaps for haskell interactive repl
  ;; ====================================
  
  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
  (define-key haskell-mode-map [f5] 'haskell-process-load-file)

  ;; Switch to the REPL.
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  ;; Build the Cabal project.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  ;; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))
  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1)))

  ;; Set the input method to Agda if it is installed. Otherwise use the default
  (when (require 'agda-input nil :noerror)
    (set-input-method "Agda")
    (define-key haskell-mode-map (kbd "C-c r a")
      (lambda ()
        (interactive)
        (insert-char #x21D2))))

  (when (require 'comment-sections nil :noerror)
    (define-key haskell-mode-map (kbd "M-n s")
      #'comment-sections:insert-section))

  (add-to-list 'company-backends 'company-ghc)
  (custom-set-variables '(company-ghc-show-info t))

  ;; (ghc-init)
  ;; (hare-init)

  ;; lambda text replacement 
  ;; (font-lock-add-keywords
  ;;  nil `((,(concat "\\(" (regexp-quote "\\") "\\)")
  ;;         (0 (progn (compose-region (match-beginning 1) (match-end 1)
  ;;                                   ,(make-char 'greek-iso8859-7 107))
  ;;                   nil)))))
  )


(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-tags-on-save t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-type 'stack-ghci))

;; Useful to have these keybindings for .cabal files, too.
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

(message "done setting up haskell")

(provide 'setup-haskell)
;;; setup-haskell.el ends here

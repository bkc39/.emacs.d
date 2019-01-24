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
(add-hook 'haskell-mode-hook 'haskell-auto-insert-blrg-module-template)

(defun haskell-auto-insert-blrg-module-template ()
  "Insert the module template for a new BLRG module"
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (let* ((current-module-name (haskell-guess-module-name))
           (module-template-string (format
                                    blrg-new-module-template-format-string
                                    current-module-name
                                    current-module-name)))
     (progn
       (insert module-template-string)
       (goto-char (point-min))
       (forward-line 9)
       (forward-char 2)
       (insert-char ?\s)))))

(defvar blrg-new-module-template-format-string
  "--------------------------------------------------------------------------------
-- |
-- Module      : %s
-- Copyright   : (c) 2018-2019 Bot Lab, LP
-- License     : All Rights Reserved
-- Maintainer  : Ben Carriel <bkc@botlablp.com>
-- Stability   : Experimental
-- Portability : GHC
--
--
--
--------------------------------------------------------------------------------

module %s where
"
  "Template for a file that is inserted in the BLRG repo.")


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

  (setq
   haskell-process-args-ghci
   '("-ferror-spans" "-fshow-loaded-modules"))

  (setq
   haskell-process-args-cabal-repl
   '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

  (setq
   haskell-process-args-stack-ghci
   '("--ghci-options=-ferror-spans -fshow-loaded-modules"
     "--no-build" "--no-load"))

  (setq
   haskell-process-args-stack-cabal-new-repl
   '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ghc)
    (custom-set-variables '(company-ghc-show-info t)))

  (when (require 'comment-sections nil :noerror)
    (define-key haskell-mode-map (kbd "M-n s")
      #'comment-sections:insert-section)))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-tags-on-save t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-process-path-ghci "stack"))


;; Useful to have these keybindings for .cabal files, too.
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

(message "done setting up haskell")

(provide 'setup-haskell)
;;; setup-haskell.el ends here

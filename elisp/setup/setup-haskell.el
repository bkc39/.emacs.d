;;; setup-haskell.el --- Setup for Haskell mode
;;; Commentary:
;;;   Tweaks to the default haskell-mode

;;; Code:

;; Haskell
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell files" t)

;; (setq auto-mode-alist
;;       (append '(("\\.hs$" . haskell-mode)
;;                 ("\\.lhs$" . haskell-mode))
;;               auto-mode-alist))

;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
;; (add-hook 'haskell-mode-hook 'turn-on-hi2)
;; (add-hook 'haskell-mode-hook #'hindent-mode)

;; (add-hook 'haskell-mode-hook
;;           '(lambda ()
;;              (setq hindent-style "chris-done")
;;              ;; Flycheck
;;              (flycheck-select-checker 'haskell-hlint)
;;              ;; Set up hoogle
;;              (setq haskell-hoogle-command "hoogle")
;;              (define-key haskell-mode-map (kbd "M-[") 'align)
;;              (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

;;              ;; REPL keyboard shortcuts
;;              (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
;;              (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;              (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;;              (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;              (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;              (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

;; ;; For indenting entire code blocks
;; (eval-after-load "haskell-mode"
;;   '(progn
;;      (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
;;      (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

;; ;; For Interactive Haskell Mode
;; (custom-set-variables
;;  '(haskell-process-suggest-remove-import-lines t)
;;  '(haskell-process-auto-import-loaded-modules t)
;;  '(haskell-process-log)
;;  '(haskell-tags-on-save t)
;;  '(haskell-stylish-on-save t)
;;  '(haskell-process-suggest-hoogle-imports t)
;;  '(haskell-process-type 'cabal-repl))


(provide 'setup-haskell)
;;; setup-haskell.el ends here

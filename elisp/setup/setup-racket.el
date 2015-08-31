;;; setup-racket.el --- Customizations for Racket
;;; Commentary:
;;;   Setup functions for racket-mode

;;; Code:

(autoload 'racket-mode "racket-mode"
  "Major mode for editing Racket files" t)

(setq auto-mode-alist
      (append '(("\\.rkt$" . racket-mode))
              auto-mode-alist))

(add-hook 'racket-mode-hook      #'enable-paredit-mode)
(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
(add-hook 'racket-mode-hook
          '(lambda ()
             (setq tab-always-indent 'complete)))

(provide 'setup-racket)
;;; setup-racket.el ends here
